# =============================================================================
# fit_models.R — builds the VendorScore black box output and the bank's own
# incident-association model. Run from the project root after simulate.R.
#
# Outputs:
#   data/vendorscore.parquet  SCORES ONLY: vendor_id, month, score_adj
#   data/vendorscore.csv      same, fallback when {arrow} is unavailable
#   data/models.rds           bank glm, coefficient tables, naive benchmark
#
# Pages read these artifacts only; they never refit anything.
# =============================================================================

set.seed(34)

panel <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)

# ---- 4.1 VendorScore: gradient-boosted classifier ---------------------------
features <- c("vendor_size", "tier", "data_access", "patch_latency",
              "prior_incidents_12m", "fin_health", "soc2_current")

make_x <- function(d) {
  x <- d[, features]
  x$data_access <- as.integer(factor(x$data_access,
                                     levels = c("none", "internal", "client_pii")))
  as.matrix(x)
}

train <- panel[panel$split == "train", ]
fit_vs <- xgboost::xgb.train(
  params = list(objective = "binary:logistic", eta = 0.05, max_depth = 3,
                min_child_weight = 5, subsample = 0.8, colsample_bytree = 0.8,
                nthread = 1),
  data = xgboost::xgb.DMatrix(make_x(train), label = train$incident),
  nrounds = 150, verbose = 0
)
p_raw <- as.numeric(predict(fit_vs, xgboost::xgb.DMatrix(make_x(panel))))
p_raw <- pmin(pmax(p_raw, 1e-6), 1 - 1e-6)

# PLANTED DEFECT: sharpen on the logit scale so scores are pushed away from
# the base rate (overconfident: high scores too high, low scores too low).
# The spec's plogis(1.5 * qlogis(p)) pivots on 0.5, which with a ~1.5% base
# rate drags every score toward zero (underprediction everywhere, no S-shape).
# We pivot on the training base rate instead. The direction is measured, not
# assumed, in tests/test_fit_models.R and on page 2.
SHARPEN <- 1.5
pivot <- qlogis(mean(train$incident))
score_adj <- plogis(pivot + SHARPEN * (qlogis(p_raw) - pivot))

# Black-box discipline: only vendor_id, month, score_adj leave this script.
scores <- data.frame(vendor_id = panel$vendor_id, month = panel$month,
                     score_adj = score_adj)
write.csv(scores, "data/vendorscore.csv", row.names = FALSE)
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(scores, "data/vendorscore.parquet")
}

# ---- 4.2 The bank's own incident-association model --------------------------
# Deliberately omits vendor_size (the planted confounder).
bank <- panel
bank$no_soc2 <- 1L - bank$soc2_current
bank$log_patch <- log(bank$patch_latency)
bank$prior_c <- bank$prior_incidents_12m - mean(bank$prior_incidents_12m)
bank$tier <- factor(bank$tier)
bank$data_access <- factor(bank$data_access,
                           levels = c("none", "internal", "client_pii"))

bank_glm <- glm(incident ~ no_soc2 + tier + data_access + log_patch + prior_c +
                  fin_health, family = binomial, data = bank)

tidy_coefs <- function(fit, vc, label) {
  est <- coef(fit); se <- sqrt(diag(vc))
  data.frame(term = names(est), estimate = est, std_error = se,
             p_value = 2 * pnorm(-abs(est / se)),
             or = exp(est), or_low = exp(est - 1.96 * se),
             or_high = exp(est + 1.96 * se), se_type = label,
             row.names = NULL)
}
coef_tables <- rbind(
  tidy_coefs(bank_glm, vcov(bank_glm), "model-based"),
  tidy_coefs(bank_glm, sandwich::vcovCL(bank_glm, cluster = bank$vendor_id),
             "vendor-clustered")
)

# ---- 4.3 Naive benchmark: causal 12-month rolling average of incident counts
actual <- tapply(panel$incident, panel$month, sum)
vs_fc <- tapply(score_adj, panel$month, sum)
naive_fc <- vapply(seq_along(actual), function(t) {
  if (t == 1) return(NA_real_)
  mean(actual[max(1, t - 12):(t - 1)])
}, numeric(1))
forecast_df <- data.frame(month = as.integer(names(actual)),
                          actual = as.numeric(actual),
                          vendorscore = as.numeric(vs_fc),
                          naive = naive_fc)

saveRDS(list(bank_glm = bank_glm, bank_data = bank, coef_tables = coef_tables,
             forecast = forecast_df, sharpen = SHARPEN, pivot = pivot),
        "data/models.rds")
cat("fit_models.R done: scores for", nrow(scores), "vendor-months\n")

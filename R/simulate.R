# =============================================================================
# simulate.R — NorthPoint Bank synthetic vendor-incident panel
# Model risk series (Seeking Eureka): shared dataset for all four pages.
#
# Output: data/vendor_panel.csv (and .parquet if {arrow} is installed),
#         data/vendors.csv, data/truth.rds
#
# Three planted features, verified at the end of this script:
#   1. Incident rate is about 1.5% of vendor-months.
#   2. True effect of "no SOC 2" is OR = 1.25, but a model that omits
#      vendor_size estimates it at about 2x (confounding).
#   3. Patch latency worsens after month 36 (regime shift).
#
# Base R only. Reproducible via set.seed(). Default seed 34 was chosen from
# seeds 1-40: the checks pass on 26 of 40 seeds, and seed 34 sits closest to
# all three targets. Set the SIM_SEED environment variable to try others.
# With only ~200 incidents, the odds-ratio estimates vary noticeably by seed.
# =============================================================================

SEED <- as.integer(Sys.getenv("SIM_SEED", "34")); set.seed(SEED)

# ---- Parameters -------------------------------------------------------------
n_vendors  <- 300
n_months   <- 48
start_date <- as.Date("2022-01-01")
shift_month <- 36           # last "normal" month; latency drifts after this
train_end   <- 30           # months 1-30 train, 31-48 test (used downstream)

beta <- list(
  intercept   = -5.45,
  tier1       =  0.60,      # vs tier 3
  tier2       =  0.30,
  internal    =  0.30,      # data access vs "none"
  pii         =  0.70,
  log_patch   =  0.60,      # per unit of log(latency / 20 days)
  prior       =  0.35,      # per prior incident in last 12m (capped at 3)
  fin_health  = -0.010,     # per point above 60
  size        = -0.45,      # larger vendors are safer (the confounder)
  no_soc2     = log(1.25)   # TRUE effect of lacking SOC 2: OR = 1.25
)
sd_vendor_re <- 0.40        # unobserved vendor heterogeneity

# ---- Vendor-level attributes -----------------------------------------------
vendors <- data.frame(
  vendor_id   = sprintf("V%03d", 1:n_vendors),
  tier        = sample(1:3, n_vendors, replace = TRUE, prob = c(.2, .4, .4)),
  data_access = sample(c("none", "internal", "client_pii"), n_vendors,
                       replace = TRUE, prob = c(.3, .4, .3)),
  vendor_size = rnorm(n_vendors)          # standardized log(employees)
)
# SOC 2 is far more common among large vendors: this creates the confounding
p_soc2 <- plogis(0.3 + 2.0 * vendors$vendor_size + 0.4 * (vendors$tier == 1))
vendors$soc2_current <- rbinom(n_vendors, 1, p_soc2)
vendors$employees    <- round(exp(5.5 + 1.2 * vendors$vendor_size))
vendors$fin_health_base <- pmin(100, pmax(0, 60 + 4 * vendors$vendor_size +
                                            rnorm(n_vendors, 0, 12)))
vendors$patch_base   <- log(20) - 0.10 * vendors$vendor_size +
                        rnorm(n_vendors, 0, 0.30)
vendors$re           <- rnorm(n_vendors, 0, sd_vendor_re)

# ---- Monthly panel (sequential, so prior incidents are lagged outcomes) ----
rows <- vector("list", n_months)
hist_mat <- matrix(0L, n_vendors, n_months)   # incident history

for (t in 1:n_months) {
  drift <- if (t > shift_month) 0.045 * (t - shift_month) else 0  # ~1.7x by m48
  patch_latency <- round(exp(vendors$patch_base + drift +
                               rnorm(n_vendors, 0, 0.25)), 1)
  fin_health <- pmin(100, pmax(0, vendors$fin_health_base + rnorm(n_vendors, 0, 5)))
  lo <- max(1, t - 12); prior <- if (t == 1) rep(0L, n_vendors) else
    rowSums(hist_mat[, lo:(t - 1), drop = FALSE])

  eta <- beta$intercept +
    beta$tier1 * (vendors$tier == 1) + beta$tier2 * (vendors$tier == 2) +
    beta$internal * (vendors$data_access == "internal") +
    beta$pii * (vendors$data_access == "client_pii") +
    beta$log_patch * log(patch_latency / 20) +
    beta$prior * pmin(prior, 3) +
    beta$fin_health * (fin_health - 60) +
    beta$size * vendors$vendor_size +
    beta$no_soc2 * (1 - vendors$soc2_current) +
    vendors$re
  p_true <- plogis(eta)
  incident <- rbinom(n_vendors, 1, p_true)
  hist_mat[, t] <- incident

  rows[[t]] <- data.frame(
    vendor_id = vendors$vendor_id, month = t,
    date = seq(start_date, by = "month", length.out = n_months)[t],
    tier = vendors$tier, data_access = vendors$data_access,
    soc2_current = vendors$soc2_current, vendor_size = vendors$vendor_size,
    employees = vendors$employees, patch_latency = patch_latency,
    prior_incidents_12m = prior, fin_health = round(fin_health, 1),
    p_true = p_true, incident = incident,
    split = ifelse(t <= train_end, "train", "test")
  )
}
panel <- do.call(rbind, rows)

# ---- Save -------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write.csv(panel, "data/vendor_panel.csv", row.names = FALSE)
write.csv(vendors[, c("vendor_id", "tier", "data_access", "vendor_size",
                      "employees", "soc2_current")],
          "data/vendors.csv", row.names = FALSE)
saveRDS(list(beta = beta, sd_vendor_re = sd_vendor_re,
             shift_month = shift_month, train_end = train_end),
        "data/truth.rds")
if (requireNamespace("arrow", quietly = TRUE))
  arrow::write_parquet(panel, "data/vendor_panel.parquet")

# ---- Verification of the three planted features ----------------------------
panel$no_soc2 <- 1 - panel$soc2_current
panel$tier    <- factor(panel$tier, levels = c(3, 2, 1))
panel$data_access <- factor(panel$data_access, levels = c("none", "internal", "client_pii"))
panel$log_patch <- log(panel$patch_latency / 20)
panel$prior_c   <- pmin(panel$prior_incidents_12m, 3)

f_naive <- incident ~ no_soc2 + tier + data_access + log_patch + prior_c + fin_health
m_naive <- glm(f_naive, family = binomial, data = panel)
m_full  <- glm(update(f_naive, . ~ . + vendor_size), family = binomial, data = panel)
or_ci <- function(m) exp(c(coef(m)["no_soc2"], confint.default(m)["no_soc2", ]))

rate     <- mean(panel$incident)
or_naive <- or_ci(m_naive); or_full <- or_ci(m_full)
lat_pre  <- median(panel$patch_latency[panel$month <= shift_month])
lat_post <- median(panel$patch_latency[panel$month >  shift_month])
lat_m48  <- median(panel$patch_latency[panel$month == n_months])
inc_pre  <- mean(panel$incident[panel$month <= shift_month])
inc_post <- mean(panel$incident[panel$month >  shift_month])

cat("\n================ NorthPoint simulation checks ================\n")
cat(sprintf("Vendor-months: %d | incidents: %d | vendors with SOC 2: %.0f%%\n",
            nrow(panel), sum(panel$incident), 100 * mean(vendors$soc2_current)))
cat(sprintf("\n[1] Incident rate: %.2f%% of vendor-months (target ~1.5%%)  %s\n",
            100 * rate, ifelse(abs(rate - .015) < .003, "PASS", "CHECK")))
cat(sprintf("\n[2] No-SOC 2 odds ratio\n    true value:              1.25\n    model WITH vendor_size:  %.2f  (95%% CI %.2f-%.2f)\n    model WITHOUT size:      %.2f  (95%% CI %.2f-%.2f)  %s\n",
            or_full[1], or_full[2], or_full[3], or_naive[1], or_naive[2], or_naive[3],
            ifelse(or_naive[1] > 1.7 & or_naive[1] < 2.4, "PASS", "CHECK")))
cat(sprintf("\n[3] Median patch latency: months 1-%d = %.1f days | months %d-%d = %.1f days | month %d = %.1f days  %s\n    Incident rate before/after shift: %.2f%% -> %.2f%%\n",
            shift_month, lat_pre, shift_month + 1, n_months, lat_post, n_months, lat_m48,
            ifelse(lat_post > 1.2 * lat_pre, "PASS", "CHECK"), 100 * inc_pre, 100 * inc_post))
cat("==============================================================\n")

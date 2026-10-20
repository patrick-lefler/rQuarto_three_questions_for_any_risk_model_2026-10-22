# Page 2 computations: are VendorScore's probabilities honest?
# Calibrators are fit on months 1-30; every reported metric is from 31-48.
# Run from the project root after fit_models.R. Writes data/calibration.rds.

library(probcal)
set.seed(34)

panel  <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)
scores <- read.csv("data/vendorscore.csv")
d      <- merge(panel[, c("vendor_id", "month", "incident", "p_true")], scores,
                by = c("vendor_id", "month"))
fitw   <- d[d$month <= 30, ]
evalw  <- d[d$month > 30, ]

iso <- cal_isotonic(fitw$score_adj, fitw$incident)
pla <- cal_platt(fitw$score_adj, fitw$incident)
evalw$isotonic <- predict(iso, evalw$score_adj)
evalw$platt    <- predict(pla, evalw$score_adj)

# Reliability data: equal-mass bins on each score's own distribution
reliability <- function(p, y, label, bins = 10) {
  br <- unique(quantile(p, seq(0, 1, length.out = bins + 1)))
  bin <- cut(p, br, include.lowest = TRUE, labels = FALSE)
  do.call(rbind, lapply(sort(unique(bin)), function(b) {
    i <- bin == b; n <- sum(i); k <- sum(y[i])
    ci <- binom.test(k, n)$conf.int
    data.frame(score = label, bin = b, n = n, predicted = mean(p[i]),
               observed = k / n, low = ci[1], high = ci[2])
  }))
}
rel <- rbind(reliability(evalw$score_adj, evalw$incident, "Raw VendorScore"),
             reliability(evalw$isotonic, evalw$incident, "Isotonic-calibrated"))

brier <- function(p) mean((p - evalw$incident)^2)
metrics <- do.call(rbind, lapply(
  list(`Raw VendorScore` = evalw$score_adj, `Isotonic-calibrated` = evalw$isotonic,
       `Platt-calibrated` = evalw$platt),
  function(p) data.frame(
    brier = brier(p),
    ece_mass = ece(p, evalw$incident, bins = 10, strategy = "mass"),
    ece_width = ece(p, evalw$incident, bins = 10, strategy = "width"),
    expected_per_month = sum(p) / 18)))
metrics$method <- rownames(metrics); rownames(metrics) <- NULL
observed_per_month <- sum(evalw$incident) / 18
p_true_per_month   <- sum(evalw$p_true) / 18

# Kernel calibration test of the vendor's "well-calibrated" claim (raw scores)
claim_test <- cal_test(evalw$score_adj, evalw$incident, n_boot = 999)

# Uncertainty on the raw-vs-isotonic differences (bootstrap over vendors)
vids <- unique(evalw$vendor_id)
boot <- replicate(500, {
  s <- sample(vids, replace = TRUE)
  b <- do.call(rbind, lapply(s, function(v) evalw[evalw$vendor_id == v, ]))
  c(d_brier = mean((b$isotonic - b$incident)^2) - mean((b$score_adj - b$incident)^2),
    d_ece_mass = ece(b$isotonic, b$incident, strategy = "mass") -
                 ece(b$score_adj, b$incident, strategy = "mass"),
    d_ece_width = ece(b$isotonic, b$incident, strategy = "width") -
                  ece(b$score_adj, b$incident, strategy = "width"))
})
boot_ci <- apply(boot, 1, quantile, c(0.025, 0.5, 0.975))

# Tier movement: vendor's mean evaluation-window score, absolute thresholds
# (assumption: Low < 1%, Medium 1-3%, High > 3% monthly incident probability)
tier_of <- function(p) cut(p, c(-Inf, 0.01, 0.03, Inf), c("Low", "Medium", "High"))
vend <- aggregate(cbind(score_adj, isotonic) ~ vendor_id, evalw, mean)
vend$tier_raw <- tier_of(vend$score_adj)
vend$tier_cal <- tier_of(vend$isotonic)
vend$move <- ifelse(as.integer(vend$tier_cal) > as.integer(vend$tier_raw), "Moves up",
             ifelse(as.integer(vend$tier_cal) < as.integer(vend$tier_raw), "Moves down",
                    "No change"))
moves <- as.data.frame(table(from = vend$tier_raw, to = vend$tier_cal, move = vend$move))
moves <- moves[moves$Freq > 0, ]

saveRDS(list(fit_months = 1:30, eval_months = 31:48, reliability = rel,
             metrics = metrics, observed_per_month = observed_per_month,
             p_true_per_month = p_true_per_month, claim_test = claim_test,
             boot_ci = boot_ci, vendor_tiers = vend, moves = moves,
             base_rate_true = mean(evalw$p_true), base_rate_obs = mean(evalw$incident)),
        "data/calibration.rds")
cat("analysis_calibration.R done\n")

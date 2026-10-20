# Page 2 computations: are VendorScore's probabilities honest, and does a
# recalibration overlay help? Windows (R/windows.R):
#   classifier trained on months 1-30 (the vendor's rows);
#   overlay fitted on months 31-36 (scores out of sample for the classifier);
#   overlay assessed on months 37-48 only.
# Method choices are fixed BEFORE looking at the evaluation window: isotonic
# (primary) and Platt on the logit scale (secondary); equal-frequency ECE with
# 10 bins (primary), equal-width ECE (sensitivity). No selection on 37-48.
# Run from the project root after fit_models.R. Writes data/calibration.rds.

library(probcal)
source("R/windows.R")
set.seed(par_get("seed"))
B <- par_get("calibration", "bootstrap_draws")
BINS <- par_get("calibration", "ece_bins")

panel  <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)
scores <- read.csv("data/vendorscore.csv")
d      <- merge(panel[, c("vendor_id", "month", "incident", "p_true")], scores,
                by = c("vendor_id", "month"))
d      <- d[order(d$vendor_id, d$month), ]
fitw   <- d[d$month %in% WINDOWS$calibration_fit, ]
evalw  <- d[d$month %in% WINDOWS$overlay_eval, ]
n_months_eval <- length(WINDOWS$overlay_eval)

stopifnot(length(intersect(WINDOWS$classifier_train, WINDOWS$calibration_fit)) == 0,
          length(intersect(WINDOWS$calibration_fit, WINDOWS$overlay_eval)) == 0)

iso <- cal_isotonic(fitw$score_adj, fitw$incident)
pla <- cal_platt(qlogis(fitw$score_adj), fitw$incident)   # logit-scale input
evalw$isotonic <- predict(iso, evalw$score_adj)
evalw$platt    <- predict(pla, qlogis(evalw$score_adj))

variants <- c(`Raw VendorScore` = "score_adj", `Isotonic overlay` = "isotonic",
              `Platt overlay (logit scale)` = "platt")

# ---- Point estimates ------------------------------------------------------------
metric_row <- function(p, y, n_months) {
  c(brier = mean((p - y)^2),
    ece_mass  = ece(p, y, bins = BINS, strategy = "mass"),
    ece_width = ece(p, y, bins = BINS, strategy = "width"),
    expected_per_month = sum(p) / n_months)
}
point <- t(vapply(variants, function(v) metric_row(evalw[[v]], evalw$incident, n_months_eval),
                  numeric(4)))
observed_per_month <- sum(evalw$incident) / n_months_eval

# ---- Vendor-cluster bootstrap ---------------------------------------------------
# Each draw resamples whole vendor histories (all 12 evaluation months of a
# vendor stay together; raw and overlay predictions stay paired). A vendor drawn
# twice enters twice as a separate copy. Conditional on the fitted overlay:
# calibrator-fitting uncertainty is NOT included. Reliability bins are fixed
# from the point sample; only membership counts are resampled.
vids <- unique(evalw$vendor_id)
rows_by_vendor <- split(seq_len(nrow(evalw)), evalw$vendor_id)

bin_breaks <- function(p, bins = BINS) {
  br <- unique(quantile(p, seq(0, 1, length.out = bins + 1), names = FALSE))
  br[1] <- -Inf; br[length(br)] <- Inf
  br
}
breaks <- lapply(variants, function(v) bin_breaks(evalw[[v]]))
bin_id <- Map(function(v, br) cut(evalw[[v]], br, labels = FALSE, right = TRUE),
              variants, breaks)

boot_one <- function() {
  idx <- cluster_resample_idx(rows_by_vendor, vids)$idx
  e <- evalw[idx, ]
  out <- list()
  for (nm in names(variants)) {
    v <- variants[[nm]]
    m <- metric_row(e[[v]], e$incident, n_months_eval)
    bi <- bin_id[[nm]][idx]
    nb <- max(bin_id[[nm]])
    obs <- vapply(seq_len(nb), function(b) mean(e$incident[bi == b]), numeric(1))
    out[[nm]] <- list(metrics = m, obs = obs)
  }
  out
}
boot <- replicate(B, boot_one(), simplify = FALSE)

q <- function(x) quantile(x, c(0.025, 0.5, 0.975), names = FALSE, na.rm = TRUE)
diff_ci <- function(nm, metric) q(vapply(boot, function(b)
  b[[nm]]$metrics[[metric]] - b[["Raw VendorScore"]]$metrics[[metric]], numeric(1)))
metric_ci <- function(nm, metric) q(vapply(boot, function(b) b[[nm]]$metrics[[metric]], numeric(1)))
metric_names <- c("brier", "ece_mass", "ece_width", "expected_per_month")

ci_tab <- do.call(rbind, lapply(names(variants), function(nm) {
  do.call(rbind, lapply(metric_names, function(m) {
    r <- metric_ci(nm, m)
    dd <- if (nm == "Raw VendorScore") c(NA, NA, NA) else diff_ci(nm, m)
    data.frame(variant = nm, metric = m, estimate = point[nm, m],
               low = r[1], high = r[3], diff_low = dd[1], diff_med = dd[2], diff_high = dd[3])
  }))
}))
rownames(ci_tab) <- NULL

# ---- Reliability data (fixed bins; clustered intervals; ties are not split) -----
rel <- do.call(rbind, lapply(names(variants), function(nm) {
  v <- variants[[nm]]; bi <- bin_id[[nm]]
  do.call(rbind, lapply(seq_len(max(bi)), function(b) {
    i <- bi == b; r <- q(vapply(boot, function(x) x[[nm]]$obs[b], numeric(1)))
    data.frame(score = nm, bin = b, n = sum(i), incidents = sum(evalw$incident[i]),
               predicted = mean(evalw[[v]][i]), observed = mean(evalw$incident[i]),
               true_mean = mean(evalw$p_true[i]), low = r[1], high = r[3])
  }))
}))

# ---- Exploratory kernel test (assumes independent rows; NOT cluster-valid) -----
claim_test <- cal_test(evalw$score_adj, evalw$incident, n_boot = 999)

# ---- Tier (risk-band) movement: retrospective illustration ----------------------
# Bands: Low < 1%, Medium >= 1% and < 3%, High >= 3% monthly incident probability
# (illustrative thresholds, distinct from the criticality `tier` column).
band_of <- function(p) cut(p, c(-Inf, par_get("calibration", "band_low_upper"), par_get("calibration", "band_medium_upper"), Inf), c("Low", "Medium", "High"), right = FALSE)
vend <- aggregate(cbind(score_adj, isotonic) ~ vendor_id, evalw, mean)
vend$band_raw <- band_of(vend$score_adj); vend$band_overlay <- band_of(vend$isotonic)
vend$move <- ifelse(as.integer(vend$band_overlay) > as.integer(vend$band_raw), "Moves up",
             ifelse(as.integer(vend$band_overlay) < as.integer(vend$band_raw), "Moves down",
                    "No change"))
moves <- as.data.frame(table(from = vend$band_raw, to = vend$band_overlay, move = vend$move))
moves <- moves[moves$Freq > 0, ]

saveRDS(list(windows = WINDOWS, window_counts = window_counts(d),
             n_eval_rows = nrow(evalw), n_eval_incidents = sum(evalw$incident),
             n_eval_vendors = length(vids), n_fit_rows = nrow(fitw),
             n_fit_incidents = sum(fitw$incident),
             reliability = rel, metrics = point, ci = ci_tab, boot_B = B,
             observed_per_month = observed_per_month,
             p_true_per_month = sum(evalw$p_true) / n_months_eval,
             claim_test = claim_test, vendor_bands = vend, moves = moves,
             calibrators = list(isotonic_input = "raw probability",
                                platt_input = "logit of raw probability"),
             ece_def = "probcal::ece, 10 bins, L1, weights n_b/n; equal-frequency (primary) and equal-width (sensitivity)"),
        "data/calibration.rds")
cat("analysis_calibration.R done\n")

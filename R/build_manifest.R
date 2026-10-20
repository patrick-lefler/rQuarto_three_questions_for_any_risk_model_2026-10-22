# Audit artifacts: data/analysis_manifest.csv (windows, models, score variants,
# feature availability), data/checksums.csv, data/session_info.txt.
# Run from the project root after the three analysis scripts.

source("R/windows.R"); source("R/baseline.R")
panel <- read.csv("data/vendor_panel.csv")
cal <- readRDS("data/calibration.rds"); fra <- readRDS("data/fragility.rds")
fct <- readRDS("data/forecast.rds")
wc <- window_counts(merge(panel, read.csv("data/vendorscore.csv"), by = c("vendor_id", "month")))

row <- function(section, item, value, note = "") data.frame(section, item, value, note)
man <- rbind(
  do.call(rbind, lapply(seq_len(nrow(wc)), function(i) with(wc[i, ], row(
    "window", window, sprintf("months %s; %d vendor-months; %d vendors; %d incidents", months, vendor_months, vendors, incidents),
    switch(window,
      classifier_train = "VendorScore (xgboost) trained here: rows of the vendor's business",
      calibration_fit  = "overlay (isotonic, Platt on logit scale) fitted here; classifier did not train on these rows",
      overlay_eval     = "final overlay assessment (page 2); not used for any fitting or method choice",
      forecast_eval    = "raw VendorScore vs 12-month average (page 3)"))))),
  row("model", "VendorScore", "xgboost binary:logistic; features vendor_size, tier, data_access, patch_latency, prior_incidents_12m, fin_health, soc2_current; labels = incident, months 1-30; scores sharpened x1.5 on the logit scale around the training base rate", "planted overconfidence"),
  row("model", BASELINE_ID, paste(deparse(bank_formula), collapse = ""), "bank's own incident-association model; full panel; omits vendor_size"),
  row("model", "bank_baseline_v2_plus_size", "baseline + vendor_size (specification S02)", "size-adjusted comparison"),
  row("score_variant", "raw", "score_adj", "vendor output"),
  row("score_variant", "isotonic", "probcal::cal_isotonic on raw probability", "fixed in advance; primary overlay"),
  row("score_variant", "platt", "probcal::cal_platt on logit(raw probability)", "fixed in advance; secondary overlay"),
  row("inference", "clustering", CLUSTER_VCOV, "unit = vendor"),
  row("inference", "calibration bootstrap", sprintf("%d vendor-history resamples, paired raw/overlay, conditional on fitted overlay", cal$boot_B), "reliability bins fixed from the point sample"),
  row("inference", "dm_test", paste(names(fct$dm_args), unlist(fct$dm_args), sep = "=", collapse = ", "), paste("forecastdom", fct$pkg_version)),
  row("inference", "falsifyr", paste(names(fra$falsifyr_args), unlist(fra$falsifyr_args), sep = "=", collapse = ", "), paste("falsifyr", fra$falsifyr_version)),
  row("inference", "greedy deletion", sprintf("budget %d; shortlist %d by dfbeta; candidates = incident rows at vendors without SOC 2; analysis-dataset deletion, lagged prior_c retained; stop at clustered p >= 0.05", fra$budget, fra$n_cand), "seed 34"),
  row("forecast", "origin", "start of month t", "target = incidents during month t; benchmark = mean of actual counts months t-12..t-1"),
  row("feature_availability", "soc2_current, tier, data_access, vendor_size", "static vendor attributes", "known before month t"),
  row("feature_availability", "prior_incidents_12m", "incidents in months t-12..t-1", "lagged outcomes only; no month-t outcome"),
  row("feature_availability", "patch_latency, fin_health", "month-t values", "ASSUMED observed at start of month t (generator draws them with month t); if an end-of-month measure, this is a contemporaneous assessment, not a one-step-ahead forecast"),
  row("generator", "simulate.R md5", fra$simulate_md5, "seed 34 default; seed selected during development from seeds 1-40 (26/40 pass checks)")
)
write.csv(man, "data/analysis_manifest.csv", row.names = FALSE)

files <- c(list.files("data", full.names = TRUE), "R/simulate.R", "params.yaml")
files <- files[!grepl("checksums|session_info", files)]
write.csv(data.frame(file = files, md5 = unname(tools::md5sum(files))), "data/checksums.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "data/session_info.txt")
cat("build_manifest.R done\n")

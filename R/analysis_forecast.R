# Page 3 computations: does raw VendorScore beat a 12-month rolling average?
# Forecast origin: START OF MONTH t. Scores use month-t covariates, assumed
# observed at the start of month t (prior_incidents_12m covers months t-12..t-1
# only, so no outcome leakage; see data/analysis_manifest.csv). The benchmark
# is the mean of actual counts in months t-12..t-1. Both forecast the same
# target month. Raw VendorScore is used: no overlay, so no fitting touches the
# target months. Nothing is refit here.
# Run from the project root after fit_models.R. Writes data/forecast.rds.

library(forecastdom)
source("R/windows.R")

fc <- readRDS("data/models.rds")$forecast
actual <- fc$actual
# Re-derive the benchmark independently as a causal check
bench_check <- rolling_benchmark(actual)
stopifnot(isTRUE(all.equal(fc$naive, bench_check)))

test <- fc[fc$month %in% WINDOWS$forecast_eval, ]
test$e_naive <- test$actual - test$naive
test$e_vs    <- test$actual - test$vendorscore
test$loss_diff <- test$e_naive^2 - test$e_vs^2   # > 0: VendorScore has lower squared error
test$cum_diff  <- cumsum(test$loss_diff)         # cumulative squared-incident loss
test$forecast_origin <- "start of month"
test$target_month <- test$month

# Explicit test settings. e1 = benchmark errors, e2 = VendorScore errors, so a
# positive statistic means lower squared loss for VendorScore. Two-sided,
# because no direction was fixed beforehand. With h = 1 the package's variance
# is the sample variance of the loss differential divided by n (no
# autocorrelation adjustment); HLN small-sample correction on, t(n - 1).
DM_ARGS <- list(h = 1L, loss = "SE", alternative = "two.sided", correction = TRUE)
summarise_window <- function(label, lo) {
  w <- test[test$month >= lo, ]
  dm <- do.call(dm_test, c(list(w$e_naive, w$e_vs), DM_ARGS))
  data.frame(window = label, n = nrow(w),
             mse_naive = mean(w$e_naive^2), mse_vs = mean(w$e_vs^2),
             mae_naive = mean(abs(w$e_naive)), mae_vs = mean(abs(w$e_vs)),
             bias_naive = mean(w$e_naive), bias_vs = mean(w$e_vs),
             dm_stat = dm$statistic, dm_p = dm$pvalue)
}
results <- rbind(summarise_window("Months 31-48", 31),
                 summarise_window("Months 37-48", 37))

saveRDS(list(test = test, results = results, shift_month = par_get("forecast", "shift_month"),
             dm_args = DM_ARGS, window = WINDOWS$forecast_eval,
             pkg_version = as.character(packageVersion("forecastdom"))),
        "data/forecast.rds")
cat("analysis_forecast.R done\n")

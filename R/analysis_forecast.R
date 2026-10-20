# Page 3 computations: does VendorScore beat a 12-month rolling average?
# Reads the cached forecasts from models.rds; nothing is refit.
# Run from the project root after fit_models.R. Writes data/forecast.rds.

library(forecastdom)

fc <- readRDS("data/models.rds")$forecast
test <- fc[fc$month >= 31, ]
test$e_naive <- test$actual - test$naive
test$e_vs    <- test$actual - test$vendorscore
test$loss_diff <- test$e_naive^2 - test$e_vs^2   # > 0: VendorScore does better
test$cum_diff  <- cumsum(test$loss_diff)

summarise_window <- function(label, lo) {
  w <- test[test$month >= lo, ]
  dm <- dm_test(w$e_naive, w$e_vs, loss = "SE")  # e1 = benchmark, e2 = VendorScore
  data.frame(window = label, n = nrow(w),
             mse_naive = mean(w$e_naive^2), mse_vs = mean(w$e_vs^2),
             mae_naive = mean(abs(w$e_naive)), mae_vs = mean(abs(w$e_vs)),
             bias_naive = mean(w$e_naive), bias_vs = mean(w$e_vs),
             dm_stat = dm$statistic, dm_p = dm$pvalue)
}
results <- rbind(summarise_window("Months 31-48 (full test window)", 31),
                 summarise_window("Months 37-48 (after the shift)", 37))

# Pre-shift part of the test window, for the cumulative-differential story
pre <- test[test$month <= 36, ]
pre_mse <- c(naive = mean(pre$e_naive^2), vendorscore = mean(pre$e_vs^2))

# Bank-side discrimination check (vendor-month level) for the claimed AUC of 0.81
auc <- function(score, y) {
  r <- rank(score); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
d <- merge(read.csv("data/vendor_panel.csv")[, c("vendor_id", "month", "incident")],
           read.csv("data/vendorscore.csv"), by = c("vendor_id", "month"))
auc_eval <- c(all = auc(d$score_adj[d$month >= 31], d$incident[d$month >= 31]),
              pre = auc(d$score_adj[d$month >= 31 & d$month <= 36], d$incident[d$month >= 31 & d$month <= 36]),
              post = auc(d$score_adj[d$month >= 37], d$incident[d$month >= 37]))

saveRDS(list(test = test, results = results, pre_mse = pre_mse, shift_month = 36,
             auc_eval = auc_eval),
        "data/forecast.rds")
cat("analysis_forecast.R done\n")

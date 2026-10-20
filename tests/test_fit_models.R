# Contract tests for the artifacts built by R/fit_models.R.
# Run from the project root: Rscript tests/test_fit_models.R
library(testthat)

panel  <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)
scores <- if (requireNamespace("arrow", quietly = TRUE)) {
  as.data.frame(arrow::read_parquet("data/vendorscore.parquet"))
} else read.csv("data/vendorscore.csv")
models <- readRDS("data/models.rds")
d      <- merge(panel, scores, by = c("vendor_id", "month"))
evalw  <- d[d$month > 30, ]

test_that("vendorscore export is scores only and complete", {
  expect_identical(names(scores), c("vendor_id", "month", "score_adj"))
  expect_equal(nrow(scores), 300 * 48)
  expect_equal(length(unique(scores$vendor_id)), 300)
  expect_setequal(unique(scores$month), 1:48)
  expect_true(all(scores$score_adj > 0 & scores$score_adj < 1))
})

test_that("planted overconfidence is measured on the evaluation window", {
  dec <- cut(evalw$score_adj, quantile(evalw$score_adj, 0:10 / 10),
             include.lowest = TRUE, labels = FALSE)
  expect_gt(mean(evalw$score_adj[dec == 10]), mean(evalw$incident[dec == 10]))
  expect_lt(mean(evalw$score_adj[dec == 1]),  mean(evalw$incident[dec == 1]))
})

test_that("bank model omits vendor_size and uses the full panel", {
  expect_false("vendor_size" %in% all.vars(formula(models$bank_glm)))
  expect_equal(nobs(models$bank_glm), 14400)
  ct <- models$coef_tables
  expect_setequal(unique(ct$se_type), c("model-based", "vendor-clustered"))
  ors <- ct$or[ct$term == "no_soc2"]
  expect_true(all(ors > 1.8 & ors < 2.3))  # naive OR ~2.0 (spec section 2)
})

test_that("naive forecast is causal", {
  f <- models$forecast
  expect_true(is.na(f$naive[1]))
  for (t in c(2, 13, 31, 48)) {
    expect_equal(f$naive[t], mean(f$actual[max(1, t - 12):(t - 1)]))
  }
  expect_equal(f$vendorscore, as.numeric(tapply(scores$score_adj, scores$month, sum)))
})

test_that("calibrator is fit on months 1-30 and evaluated on 31-48", {
  skip_if_not(file.exists("data/calibration.rds"), "run R/analysis_calibration.R first")
  cal <- readRDS("data/calibration.rds")
  expect_equal(cal$fit_months, 1:30)
  expect_equal(cal$eval_months, 31:48)
})

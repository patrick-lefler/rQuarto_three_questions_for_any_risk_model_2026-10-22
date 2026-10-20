# Contract tests for the artifacts built by the R/ scripts.
# Run from the project root: Rscript tests/test_fit_models.R
library(testthat)
source("R/windows.R"); source("R/baseline.R")

panel  <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)
scores <- if (requireNamespace("arrow", quietly = TRUE)) {
  as.data.frame(arrow::read_parquet("data/vendorscore.parquet"))
} else read.csv("data/vendorscore.csv")
models <- readRDS("data/models.rds")
cal <- readRDS("data/calibration.rds"); fra <- readRDS("data/fragility.rds")
fct <- readRDS("data/forecast.rds")
d      <- merge(panel, scores, by = c("vendor_id", "month"))
evalw  <- d[d$month %in% WINDOWS$overlay_eval, ]

test_that("vendorscore export is scores only and complete", {
  expect_identical(names(scores), c("vendor_id", "month", "score_adj"))
  expect_equal(nrow(scores), 300 * 48)
  expect_equal(length(unique(scores$vendor_id)), 300)
  expect_setequal(unique(scores$month), 1:48)
  expect_true(all(scores$score_adj > 0 & scores$score_adj < 1))
})

test_that("planted overconfidence is measured on the overlay evaluation window", {
  dec <- cut(evalw$score_adj, quantile(evalw$score_adj, 0:10 / 10),
             include.lowest = TRUE, labels = FALSE)
  expect_gt(mean(evalw$score_adj[dec == 10]), mean(evalw$incident[dec == 10]))
  expect_lt(mean(evalw$score_adj[dec == 1]),  mean(evalw$incident[dec == 1]))
})

test_that("windows are disjoint where fitting meets evaluation", {
  expect_length(intersect(WINDOWS$classifier_train, WINDOWS$calibration_fit), 0)
  expect_length(intersect(WINDOWS$calibration_fit, WINDOWS$overlay_eval), 0)
  expect_length(intersect(WINDOWS$classifier_train, WINDOWS$overlay_eval), 0)
  expect_equal(cal$windows$overlay_eval, 37:48)
  expect_equal(cal$n_eval_rows, 300 * 12)
  expect_equal(cal$n_eval_incidents, sum(evalw$incident))
  expect_equal(sum(cal$reliability$n[cal$reliability$score == "Raw VendorScore"]), cal$n_eval_rows)
})

test_that("bank model uses the canonical baseline and omits vendor_size", {
  expect_false("vendor_size" %in% all.vars(formula(models$bank_glm)))
  expect_equal(nobs(models$bank_glm), 14400)
  expect_equal(levels(models$bank_data$tier), c("3", "2", "1"))
  expect_equal(models$bank_data$prior_c, pmin(panel$prior_incidents_12m, 3))
  # validated simulate.R check: naive OR 2.04 (95% CI 1.50-2.79)
  or <- exp(coef(models$bank_glm)["no_soc2"])
  expect_equal(unname(or), 2.04, tolerance = 0.005)
  ct <- models$coef_tables
  expect_setequal(unique(ct$se_type), c("model-based", "vendor-clustered"))
})

test_that("one baseline feeds every page", {
  b <- term_clustered(models$bank_glm, models$bank_data$vendor_id)
  s01 <- fra$curve[fra$curve$vendor_size == FALSE & fra$curve$prior & fra$curve$fin_health & !fra$curve$trim, ]
  expect_equal(fra$base$or, b$or); expect_equal(fra$base$p, b$p)
  expect_equal(s01$or, b$or); expect_equal(s01$p, b$p)
  expect_equal(fra$deletion_path$or[1], b$or)
  s34 <- fra$seed_or[fra$seed_or$seed == 34, ]
  expect_equal(s34$naive_or, b$or); expect_equal(s34$adj_or, fra$plus_size$or)
  expect_equal(fra$n_rows, nrow(panel)); expect_equal(fra$n_incidents, sum(panel$incident))
})

test_that("specification curve is logit odds ratios with saved manifest rows", {
  man <- read.csv("data/specification_manifest.csv")
  expect_equal(nrow(man), 16); expect_equal(nrow(fra$curve), 16)
  expect_true(all(man$link == "logit"))
  expect_true(all(man$effect_type == "odds ratio (logit)"))
  expect_setequal(man$spec_id, fra$curve$spec_id)
})

test_that("naive forecast is causal and matches a hand-worked example", {
  expect_equal(rolling_benchmark(c(2, 4, 6, 8)), c(NA, 2, 3, 4))
  x <- c(1:30); x2 <- x; x2[20:30] <- 999            # change outcomes from month 20 on
  expect_equal(rolling_benchmark(x)[1:20], rolling_benchmark(x2)[1:20])
  f <- models$forecast
  expect_equal(f$naive, rolling_benchmark(f$actual))
  expect_equal(f$vendorscore, as.numeric(tapply(scores$score_adj, scores$month, sum)))
})

test_that("dm_test sign convention and two-sided symmetry", {
  e_bad <- c(3, -3, 2, -4, 3, -2, 4, -3); e_good <- c(1, -1, 1, -1, 1, -1, 1, -1)
  a <- do.call(forecastdom::dm_test, c(list(e_bad, e_good), fct$dm_args))
  b <- do.call(forecastdom::dm_test, c(list(e_good, e_bad), fct$dm_args))
  expect_gt(a$statistic, 0)                       # e2 (second) has smaller loss
  expect_equal(a$statistic, -b$statistic)
  expect_equal(a$pvalue, b$pvalue)
})

test_that("cumulative advantage equals summed benchmark minus VendorScore loss", {
  t <- fct$test
  expect_equal(t$cum_diff, cumsum((t$actual - t$naive)^2 - (t$actual - t$vendorscore)^2))
  expect_equal(fct$window, 31:48)
})

test_that("cluster resampling carries whole vendor histories", {
  set.seed(1)
  rows <- split(seq_len(nrow(evalw)), evalw$vendor_id)
  r <- cluster_resample_idx(rows)
  # every drawn vendor contributes all 12 of its months, each draw once
  expect_true(all(table(r$draw) == 12))
  expect_equal(length(r$idx), 300 * 12)
  for (dd in unique(r$draw)[1:20]) expect_length(unique(evalw$vendor_id[r$idx[r$draw == dd]]), 1)
})

test_that("manifests and artifacts exist", {
  for (f in c("data/analysis_manifest.csv", "data/specification_manifest.csv",
              "data/seed_sweep.csv", "data/deletion_path.csv", "data/checksums.csv",
              "data/session_info.txt")) expect_true(file.exists(f), info = f)
  expect_equal(nrow(read.csv("data/seed_sweep.csv")), 40)
})

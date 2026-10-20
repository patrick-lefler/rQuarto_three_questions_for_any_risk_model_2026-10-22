# =============================================================================
# test_simulate.R — tests for R/simulate.R
# Run from the project root:   Rscript tests/test_simulate.R
#
# Base R only (no testthat needed). Runs simulate.R in temporary folders,
# so it never overwrites data/. Exits with status 1 if any test fails.
# =============================================================================

script <- normalizePath("R/simulate.R", mustWork = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")

# Run simulate.R in a fresh temp folder (system2 has no wd argument)
run_sim <- function(seed = 34) {
  dir <- tempfile("sim_"); dir.create(dir)
  old <- setwd(dir); on.exit(setwd(old))
  out <- system2(rscript, script, stdout = TRUE, stderr = TRUE,
                 env = paste0("SIM_SEED=", seed))
  list(dir = dir, out = out,
       panel   = read.csv(file.path(dir, "data/vendor_panel.csv"), stringsAsFactors = FALSE),
       vendors = read.csv(file.path(dir, "data/vendors.csv"), stringsAsFactors = FALSE),
       truth   = readRDS(file.path(dir, "data/truth.rds")))
}

# ---- Minimal test harness ---------------------------------------------------
results <- data.frame(group = character(), test = character(), ok = logical(),
                      msg = character(), stringsAsFactors = FALSE)
test <- function(group, name, expr) {
  res <- tryCatch({ ok <- isTRUE(expr); list(ok = ok, msg = if (ok) "" else "returned FALSE") },
                  error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  results[nrow(results) + 1, ] <<- list(group, name, res$ok, res$msg)
  cat(sprintf("  [%s] %s\n", if (res$ok) "PASS" else "FAIL", name))
}

cat("Running simulate.R (seed 34)...\n")
s <- run_sim(34); p <- s$panel; v <- s$vendors

# ---- 1. Structure -----------------------------------------------------------
cat("\n1. Structure\n")
expected_cols <- c("vendor_id", "month", "date", "tier", "data_access", "soc2_current",
                   "vendor_size", "employees", "patch_latency", "prior_incidents_12m",
                   "fin_health", "p_true", "incident", "split")
test("structure", "panel has 14,400 rows (300 x 48)", nrow(p) == 14400)
test("structure", "panel has the expected columns", identical(names(p), expected_cols))
test("structure", "300 unique vendors", length(unique(p$vendor_id)) == 300 && nrow(v) == 300)
test("structure", "months run 1-48", identical(sort(unique(p$month)), 1:48))
test("structure", "one row per vendor-month", !any(duplicated(p[, c("vendor_id", "month")])))
test("structure", "no missing values", !anyNA(p))
test("structure", "dates run Jan 2022 - Dec 2025",
     min(as.Date(p$date)) == as.Date("2022-01-01") && max(as.Date(p$date)) == as.Date("2025-12-01"))
test("structure", "split: months 1-30 train, 31-48 test",
     all(p$split[p$month <= 30] == "train") && all(p$split[p$month > 30] == "test"))

# ---- 2. Valid values --------------------------------------------------------
cat("\n2. Valid values\n")
test("values", "incident is 0/1", all(p$incident %in% 0:1))
test("values", "soc2_current is 0/1", all(p$soc2_current %in% 0:1))
test("values", "p_true strictly between 0 and 1", all(p$p_true > 0 & p$p_true < 1))
test("values", "tier is 1-3", all(p$tier %in% 1:3))
test("values", "data_access has 3 known levels",
     setequal(unique(p$data_access), c("none", "internal", "client_pii")))
test("values", "fin_health within 0-100", all(p$fin_health >= 0 & p$fin_health <= 100))
test("values", "patch_latency positive", all(p$patch_latency > 0))
test("values", "employees positive", all(p$employees > 0))

# ---- 3. Internal consistency ------------------------------------------------
cat("\n3. Internal consistency\n")
const_within <- function(col) all(tapply(p[[col]], p$vendor_id, function(x) length(unique(x)) == 1))
test("consistency", "vendor attributes constant over time",
     all(sapply(c("tier", "data_access", "soc2_current", "vendor_size", "employees"), const_within)))
test("consistency", "vendors.csv matches panel attributes", {
  m <- merge(v, unique(p[, c("vendor_id", "tier", "soc2_current")]), by = "vendor_id")
  nrow(m) == 300 && all(m$tier.x == m$tier.y) && all(m$soc2_current.x == m$soc2_current.y)
})
test("consistency", "prior_incidents_12m equals incidents in previous 12 months", {
  p2 <- p[order(p$vendor_id, p$month), ]
  calc <- ave(p2$incident, p2$vendor_id, FUN = function(x)
    sapply(seq_along(x), function(t) if (t == 1) 0 else sum(x[max(1, t - 12):(t - 1)])))
  all(calc == p2$prior_incidents_12m)
})
test("consistency", "month 1 has no prior incidents", all(p$prior_incidents_12m[p$month == 1] == 0))
test("consistency", "truth.rds holds true no-SOC 2 effect log(1.25)",
     abs(s$truth$beta$no_soc2 - log(1.25)) < 1e-12 && s$truth$shift_month == 36 && s$truth$train_end == 30)
test("consistency", "observed incidents match p_true (calibrated truth, within 3 SE)", {
  se <- sqrt(sum(p$p_true * (1 - p$p_true)))
  abs(sum(p$incident) - sum(p$p_true)) < 3 * se
})

# ---- 4. Reproducibility -----------------------------------------------------
cat("\n4. Reproducibility\n")
s_again <- run_sim(34); s_other <- run_sim(35)
test("reproducibility", "same seed gives identical panel", identical(s$panel, s_again$panel))
test("reproducibility", "different seed gives different panel", !identical(s$panel$incident, s_other$panel$incident))

# ---- 5. The three planted features -----------------------------------------
cat("\n5. Planted features\n")
fit_or <- function(p, with_size) {
  p$no_soc2 <- 1 - p$soc2_current
  p$tier <- factor(p$tier, levels = c(3, 2, 1))
  p$data_access <- factor(p$data_access, levels = c("none", "internal", "client_pii"))
  p$log_patch <- log(p$patch_latency / 20); p$prior_c <- pmin(p$prior_incidents_12m, 3)
  f <- incident ~ no_soc2 + tier + data_access + log_patch + prior_c + fin_health
  if (with_size) f <- update(f, . ~ . + vendor_size)
  unname(exp(coef(glm(f, family = binomial, data = p))["no_soc2"]))
}
rate <- mean(p$incident); or_naive <- fit_or(p, FALSE); or_full <- fit_or(p, TRUE)
pre  <- p$month <= 36

test("planted", sprintf("[1] incident rate ~1.5%% (got %.2f%%)", 100 * rate), abs(rate - 0.015) < 0.003)
test("planted", sprintf("[2] naive no-SOC 2 OR ~2x (got %.2f)", or_naive), or_naive > 1.7 && or_naive < 2.4)
test("planted", sprintf("[2] size-adjusted OR near 1.25 (got %.2f)", or_full), or_full > 1.0 && or_full < 1.6)
test("planted", "[2] SOC 2 more common among larger vendors (confounding exists)",
     cor(v$vendor_size, v$soc2_current) > 0.3)
test("planted", "[3] median patch latency rises >20% after month 36",
     median(p$patch_latency[!pre]) > 1.2 * median(p$patch_latency[pre]))
test("planted", "[3] latency stable before the shift (monthly medians within 10%)", {
  m <- tapply(p$patch_latency[pre], p$month[pre], median); max(m) / min(m) < 1.10
})
test("planted", "[3] incident rate higher after the shift", mean(p$incident[!pre]) > mean(p$incident[pre]))

# ---- 6. Robustness across seeds (planted features hold in most runs) -------
cat("\n6. Robustness across 10 seeds (expect most to pass)\n")
seeds <- 1:10
rob <- t(sapply(seeds, function(sd) {
  q <- run_sim(sd)$panel
  c(rate = mean(q$incident), naive = fit_or(q, FALSE),
    shift = median(q$patch_latency[q$month > 36]) / median(q$patch_latency[q$month <= 36]))
}))
test("robustness", sprintf("rate within 1.0-2.0%% on all seeds (range %.2f-%.2f%%)",
                           100 * min(rob[, "rate"]), 100 * max(rob[, "rate"])),
     all(rob[, "rate"] > 0.010 & rob[, "rate"] < 0.020))
test("robustness", sprintf("naive OR > 1.5 on all seeds (median %.2f)", median(rob[, "naive"])),
     all(rob[, "naive"] > 1.5))
test("robustness", "patch latency shift present on all seeds", all(rob[, "shift"] > 1.2))

# ---- Summary ----------------------------------------------------------------
n_fail <- sum(!results$ok)
cat(sprintf("\n==== %d tests, %d passed, %d failed ====\n", nrow(results), sum(results$ok), n_fail))
if (n_fail > 0) { print(results[!results$ok, c("group", "test", "msg")], row.names = FALSE); quit(status = 1) }

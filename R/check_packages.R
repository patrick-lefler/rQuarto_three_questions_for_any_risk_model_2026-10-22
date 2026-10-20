# Gate 0 — verify probcal, forecastdom, falsifyr against the shared panel.
# Run from the project root after fit_models.R. Writes data/package_check.txt.

out <- character()
log_line <- function(...) out <<- c(out, paste0(...))

panel  <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)
scores <- read.csv("data/vendorscore.csv")
d      <- merge(panel, scores, by = c("vendor_id", "month"))
fitw   <- d[d$month <= 30, ]
evalw  <- d[d$month > 30, ]
models <- readRDS("data/models.rds")

check <- function(pkg, need, fun) {
  log_line("== ", pkg, " ", as.character(packageVersion(pkg)), " — ", need)
  res <- tryCatch(fun(), error = function(e) paste("FAILED:", conditionMessage(e)))
  log_line(res)
  log_line("")
}

check("probcal", "isotonic + Platt calibrators, ECE, Brier", function() {
  iso <- probcal::cal_isotonic(fitw$score_adj, fitw$incident)
  pl  <- probcal::cal_platt(fitw$score_adj, fitw$incident)
  p_iso <- predict(iso, evalw$score_adj); p_pl <- predict(pl, evalw$score_adj)
  brier <- function(p) mean((p - evalw$incident)^2)
  sprintf("OK. Eval window ECE raw %.4f, isotonic %.4f, Platt %.4f; Brier raw %.5f, isotonic %.5f, Platt %.5f. Decision: USE probcal.",
          probcal::ece(evalw$score_adj, evalw$incident), probcal::ece(p_iso, evalw$incident),
          probcal::ece(p_pl, evalw$incident), brier(evalw$score_adj), brier(p_iso), brier(p_pl))
})

check("forecastdom", "Diebold-Mariano with HLN small-sample correction, n = 18", function() {
  f <- models$forecast[models$forecast$month > 30, ]
  r <- forecastdom::dm_test(f$actual - f$naive, f$actual - f$vendorscore)
  sprintf("OK. DM statistic %.3f, p = %.3f, n = %d (e1 = naive benchmark, e2 = VendorScore). Decision: USE forecastdom.",
          r$statistic, r$pvalue, r$n)
})

check("falsifyr", "smallest-kill analysis of the no_soc2 coefficient", function() {
  a <- falsifyr::attack(models$bank_glm, term = "no_soc2", data = models$bank_data,
                        cluster = "vendor_id", seed = 34, intensity = "normal",
                        verbose = FALSE)
  sprintf(paste0("RUNS, but verdict %s, smallest kill %s over %d attacks. Its p-values are not vendor-clustered ",
                 "(cluster only groups row deletion) and it cannot add the omitted confounder vendor_size. ",
                 "Decision: USE falsifyr for within-model perturbations; the pre-registered kill ",
                 "(clustered SEs, adding vendor_size) is a hand-built loop. Footnote the split on page 4."),
          a$verdict, if (is.null(a$smallest_kill)) "none found" else "found", nrow(a$attacks))
})

writeLines(out, "data/package_check.txt")
cat(out, sep = "\n")

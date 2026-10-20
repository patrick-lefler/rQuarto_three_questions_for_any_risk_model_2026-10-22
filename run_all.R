# One-command reproduction: simulate -> fit -> package check -> analyses ->
# manifests -> tests -> render -> word count -> link check.
# Run from the project root: Rscript run_all.R
steps <- c("R/simulate.R", "R/fit_models.R", "R/check_packages.R",
           "R/analysis_calibration.R", "R/analysis_forecast.R", "R/analysis_fragility.R",
           "R/build_manifest.R", "tests/run_tests.R")
for (s in steps) {
  cat("\n>>", s, "\n")
  status <- system2("Rscript", s)
  if (status != 0) stop("step failed: ", s)
}
unlink("_freeze", recursive = TRUE)          # force pages to re-run on the new artifacts
stopifnot(system2("quarto", "render") == 0)
stopifnot(system2("Rscript", "R/word_count.R") == 0)
stopifnot(system2("Rscript", "tests/test_pages.R") == 0)
cat("\nrun_all.R complete\n")

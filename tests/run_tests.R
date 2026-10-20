# Runs the pre-render test suites and records the counts in data/test_results.csv
# (read by the Reproducibility page). Page tests run after render and are not
# recorded here. Run from the project root: Rscript tests/run_tests.R
suites <- c(simulation = "tests/test_simulate.R", artifacts = "tests/test_fit_models.R")
rows <- lapply(names(suites), function(nm) {
  out <- suppressWarnings(system2("Rscript", suites[[nm]], stdout = TRUE, stderr = TRUE))
  failed <- !is.null(attr(out, "status")) && attr(out, "status") != 0
  passed <- if (nm == "simulation") {
    as.integer(sub(".*, (\\d+) passed.*", "\\1", grep("tests, \\d+ passed", out, value = TRUE)))
  } else {
    sum(as.integer(sub(".*with (\\d+) success.*", "\\1", grep("Test passed with \\d+ success", out, value = TRUE))))
  }
  cat(sprintf("%-11s %3d checks passed%s\n", nm, passed, if (failed) "  FAILED" else ""))
  data.frame(suite = nm, script = suites[[nm]], checks_passed = passed, status = if (failed) "failed" else "passed")
})
res <- do.call(rbind, rows)
write.csv(res, "data/test_results.csv", row.names = FALSE)
if (any(res$status == "failed")) quit(status = 1)

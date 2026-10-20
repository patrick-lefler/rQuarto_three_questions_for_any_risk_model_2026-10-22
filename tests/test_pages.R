# Page-level checks: render, word budget, figure/table limits.
# Run from the project root: Rscript tests/test_pages.R
library(testthat)

pages <- c("index.qmd", "calibration.qmd", "forecast.qmd", "fragility.qmd")

test_that("site renders without errors or warnings", {
  out <- suppressWarnings(system2("quarto", "render", stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  expect_true(is.null(status) || status == 0)
  expect_false(any(grepl("WARN|Error", out)))
  expect_true(all(file.exists(file.path("docs", sub("qmd$", "html", pages)))))
})

test_that("word count is within the ceiling", {
  status <- system2("Rscript", "R/word_count.R", stdout = FALSE)
  expect_equal(status, 0)
})

test_that("each page has <= 3 figures and <= 1 table", {
  for (p in pages) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    n_fig <- length(gregexpr("label: fig-|\\{#fig-", x)[[1]][gregexpr("label: fig-|\\{#fig-", x)[[1]] > 0])
    n_tbl <- length(gregexpr("label: tbl-", x)[[1]][gregexpr("label: tbl-", x)[[1]] > 0])
    expect_lte(n_fig, 3); expect_lte(n_tbl, 1)
  }
})

test_that("each page cites the guidance and uses non-prescriptive language", {
  for (p in pages) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_match(x, "April 2026 interagency", info = p)
    expect_false(grepl("regulators require", x, ignore.case = TRUE), info = p)
  }
})

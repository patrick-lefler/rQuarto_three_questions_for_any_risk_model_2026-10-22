# Page-level checks: render, word budget, figure/table limits, rendered output.
# Run from the project root: Rscript tests/test_pages.R
library(testthat)

pages <- c("index.qmd", "calibration.qmd", "forecast.qmd", "fragility.qmd", "reproducibility.qmd")
analysis_pages <- c("calibration.qmd", "forecast.qmd", "fragility.qmd")
html <- function(p) paste(readLines(file.path("docs", sub("qmd$", "html", p)), warn = FALSE), collapse = "\n")

test_that("site renders without errors or warnings", {
  out <- suppressWarnings(system2("quarto", "render", stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  expect_true(is.null(status) || status == 0)
  expect_false(any(grepl("WARN|Error", out) & !grepl("renv", out)))
  expect_true(all(file.exists(file.path("docs", sub("qmd$", "html", pages)))))
})

test_that("rendered word count is within the ceiling", {
  status <- system2("Rscript", "R/word_count.R", stdout = FALSE)
  expect_equal(status, 0)
})

test_that("each page has <= 3 figures and <= 1 table", {
  for (p in pages) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    n_fig <- lengths(regmatches(x, gregexpr("label: fig-|\\{#fig-", x)))
    n_tbl <- lengths(regmatches(x, gregexpr("label: tbl-", x)))
    expect_lte(n_fig, 3); expect_lte(n_tbl, 1)
  }
})

test_that("each page cites the dated guidance and uses non-prescriptive language", {
  for (p in analysis_pages) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_match(x, "April 17, 2026 interagency", info = p)
    expect_false(grepl("regulators require", x, ignore.case = TRUE), info = p)
    expect_match(x, "partial validation", ignore.case = TRUE, info = p)
  }
  idx <- paste(readLines("index.qmd", warn = FALSE), collapse = "\n")
  expect_match(idx, "April 17, 2026 interagency")
})

test_that("rendered pages have no literal NA cells, drafting headings or raw footnote markers", {
  for (p in pages) {
    h <- html(p)
    expect_false(grepl("<td[^>]*>\\s*NA\\s*</td>", h), info = p)
    expect_false(grepl("\\^[0-9]\\^", h), info = p)
    expect_false(grepl(">Hook<", h), info = p)
  }
  for (p in c("calibration.qmd", "fragility.qmd")) {
    expect_match(html(p), "footnote-back", info = p)
  }
})

test_that("every table and figure has two reader notes", {
  for (p in pages) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    n_obj <- lengths(regmatches(x, gregexpr("label: (fig|tbl)-|\\{#fig-", x)))
    n_note <- lengths(regmatches(x, gregexpr("\\{\\.reader-note\\}", x)))
    expect_equal(n_note, n_obj, info = p)
    expect_equal(lengths(regmatches(x, gregexpr("\\*\\*How to read the", x))), n_obj, info = p)
    expect_equal(lengths(regmatches(x, gregexpr("\\*\\*What the (plot|table) tells you", x))), n_obj, info = p)
  }
})

test_that("committee actions use 'sound practice suggests'", {
  for (p in analysis_pages) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_match(x, "Sound practice suggests", info = p)
  }
})

test_that("params.yaml drives the scripts and the Reproducibility page", {
  source("R/params.R"); source("R/windows.R")
  expect_equal(WINDOWS$overlay_eval, par_get("windows", "overlay_eval")[1]:par_get("windows", "overlay_eval")[2])
  expect_equal(par_get("seed"), 34)
  repro <- paste(readLines("reproducibility.qmd", warn = FALSE), collapse = "\n")
  expect_match(repro, "params.yaml"); expect_match(repro, "Limitations")
  expect_true(all(file.exists(c("params.yaml", "data/test_results.csv"))))
})

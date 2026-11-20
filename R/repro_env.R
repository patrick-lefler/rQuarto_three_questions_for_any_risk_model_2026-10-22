# Environment table for the Reproducibility page (copied from the rquarto-reproducibility skill).
# Edit only the arguments you pass in, not this file, so every project's table looks the same.

# packages: the project's key packages, shown with their installed versions.
# extra:    optional tibble(Item, Value) of project-specific rows (e.g. a pinned GitHub fork).
repro_env_table <- function(packages = character(), extra = NULL) {
  quarto <- tryCatch(
    trimws(system2("quarto", "--version", stdout = TRUE, stderr = FALSE))[1],
    error = \(e) NA_character_
  )
  renv_state <- if (file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
    st <- NULL
    utils::capture.output(st <- suppressMessages(renv::status()))
    if (isTRUE(st$synchronized)) {
      "In sync with renv.lock"
    } else {
      "Out of sync with renv.lock: run renv::status() for details"
    }
  } else {
    "Not pinned (no renv.lock)"
  }
  pkg_versions <- purrr::map_chr(packages, \(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = \(e) "not installed")
  })

  dplyr::bind_rows(
    tibble::tibble(
      Item = c("R", "Platform", "Quarto", "Packages"),
      Value = c(R.version.string, R.version$platform, quarto, renv_state)
    ),
    tibble::tibble(Item = packages, Value = pkg_versions),
    extra
  )
}

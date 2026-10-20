# Prose word-count checker. Strips YAML, code chunks and chunk options from
# each page, plus the reader-note divs (.reader-note, outside the budget); captions in #| fig-cap / tbl-cap lines count toward the budget.
# Run from the project root: Rscript R/word_count.R. Exits 1 if total > 4000.

budget <- c("index.qmd" = 750, "calibration.qmd" = 1000,
            "forecast.qmd" = 1000, "fragility.qmd" = 1000)
CEILING <- 4000

count_words <- function(path) {
  x <- readLines(path, warn = FALSE)
  fence <- grepl("^```", x)
  in_code <- cumsum(fence) %% 2 == 1 | fence
  yaml <- cumsum(x == "---") == 1 & x != "---"
  open <- grepl("^:::\\s*\\{[^}]*reader-note", x)
  in_note <- logical(length(x)); on <- FALSE
  for (i in seq_along(x)) {
    if (open[i]) on <- TRUE
    in_note[i] <- on
    if (on && !open[i] && grepl("^:::\\s*$", x[i])) on <- FALSE
  }
  caps <- x[in_code & grepl("^#\\|\\s*(fig|tbl)-cap:", x)]
  caps <- sub("^#\\|\\s*(fig|tbl)-cap:\\s*", "", caps)
  prose <- x[!in_code & !in_note & !yaml & x != "---"]
  txt <- c(prose, caps)
  txt <- gsub("\\{[#.:][^}]*\\}|<[^>]+>|\\[|\\]\\([^)]*\\)|[#*_>|`]", " ", txt)
  length(strsplit(paste(txt, collapse = " "), "\\s+")[[1]][nzchar(strsplit(paste(txt, collapse = " "), "\\s+")[[1]])])
}

pages <- names(budget)[file.exists(names(budget))]
counts <- vapply(pages, count_words, integer(1))
tab <- data.frame(page = pages, words = counts, budget = budget[pages], row.names = NULL)
print(tab)
total <- sum(counts)
cat(sprintf("Total: %d (target 3750, ceiling %d)\n", total, CEILING))
if (total > CEILING) quit(status = 1)

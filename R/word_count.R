# Reader-visible word count from the RENDERED pages (docs/*.html).
# Counts everything in the main content: title block, paragraphs, lists,
# headings, reader notes, table text, captions, callouts (including collapsed
# ones) and footnotes, with inline results as rendered. Excludes navigation
# chrome and folded source code. Run from the project root after quarto render:
#   Rscript R/word_count.R
# Exits 1 if the total exceeds the ceiling. Writes data/word_count.csv.

library(xml2)
# Ceiling raised from 4,000 to 4,100 by the owner (2026-10-21) to allow the plain-language
# key to the guidance's section marks on the Overview page.
CEILING <- 4100; TARGET <- c(3500, 3750)
pages <- c("index", "calibration", "forecast", "fragility", "reproducibility")

count_page <- function(p) {
  doc <- read_html(file.path("docs", paste0(p, ".html")))
  main <- xml_find_first(doc, "//main[@id='quarto-document-content']")
  drop <- xml_find_all(main, ".//details[contains(@class,'code-fold')] | .//script | .//style | .//div[contains(@class,'sourceCode')]")
  xml_remove(drop)
  txt <- xml_text(main)
  words <- strsplit(trimws(gsub("\\s+", " ", txt)), " ")[[1]]
  length(words[nzchar(words)])
}
counts <- vapply(pages, count_page, integer(1))
tab <- data.frame(page = paste0(pages, ".qmd"), words = counts, row.names = NULL)
print(tab)
total <- sum(counts)
cat(sprintf("Total: %d (target %d-%d, ceiling %d)\n", total, TARGET[1], TARGET[2], CEILING))
write.csv(rbind(tab, data.frame(page = "total", words = total)), "data/word_count.csv", row.names = FALSE)
if (total > CEILING) quit(status = 1)

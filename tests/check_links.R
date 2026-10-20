# Link check on the rendered site: internal files/anchors and external URLs.
# Writes qa/link_check.csv. Run after quarto render: Rscript tests/check_links.R
library(xml2)
pages <- c("index", "calibration", "forecast", "fragility", "reproducibility")
links <- do.call(rbind, lapply(pages, function(p) {
  doc <- read_html(file.path("docs", paste0(p, ".html")))
  a <- xml_attr(xml_find_all(doc, "//main//a[@href] | //footer//a[@href] | //nav//a[@href]"), "href")
  data.frame(page = p, href = unique(a))
}))
links <- unique(links)
check <- function(h, page) {
  if (grepl("^https?://", h)) {
    code <- tryCatch(as.integer(system2("curl", c("-s", "-o", "/dev/null", "-L", "-w", "%{http_code}",
                      "-A", "Mozilla/5.0", "--max-time", "20", shQuote(h)), stdout = TRUE)), error = function(e) NA)
    return(data.frame(kind = "external", status = code, ok = !is.na(code) && code >= 200 && code < 400))
  }
  if (grepl("^(mailto:|#)", h)) return(data.frame(kind = "anchor", status = NA, ok = TRUE))
  f <- sub("#.*$", "", h); f <- if (f == "") paste0(page, ".html") else f
  data.frame(kind = "internal", status = NA, ok = file.exists(file.path("docs", f)))
}
res <- do.call(rbind, Map(function(h, p) cbind(href = h, page = p, check(h, p)), links$href, links$page))
rownames(res) <- NULL
write.csv(res, "qa/link_check.csv", row.names = FALSE)
print(res[!res$ok | res$kind == "external", ])
cat(sprintf("%d links checked, %d failing\n", nrow(res), sum(!res$ok)))
if (any(!res$ok)) quit(status = 1)

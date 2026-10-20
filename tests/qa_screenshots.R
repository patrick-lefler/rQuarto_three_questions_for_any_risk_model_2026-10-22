# Visual QA: screenshots of every table and figure at desktop and mobile widths,
# plus a horizontal-overflow check. Writes qa/*.png and qa/overflow.csv.
# Run from the project root after quarto render: Rscript tests/qa_screenshots.R
library(chromote)
pages <- c("index", "calibration", "forecast", "fragility", "reproducibility")
widths <- c(desktop = 1280, mobile = 390)
b <- ChromoteSession$new()
shot <- function(path, x, y, w, h) {
  r <- b$Page$captureScreenshot(format = "png", captureBeyondViewport = TRUE,
                                clip = list(x = x, y = y, width = w, height = h, scale = 1))
  writeBin(jsonlite::base64_dec(r$data), path)
}
res <- list()
for (p in pages) for (w in names(widths)) {
  b$Emulation$setDeviceMetricsOverride(width = widths[[w]], height = 900, deviceScaleFactor = 1,
                                       mobile = (w == "mobile"))
  b$Page$navigate(paste0("file://", normalizePath(file.path("docs", paste0(p, ".html")))))
  Sys.sleep(1.5)
  m <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify({sw: document.documentElement.scrollWidth, cw: document.documentElement.clientWidth, h: document.documentElement.scrollHeight})")$result$value)
  res[[length(res) + 1]] <- data.frame(page = p, view = w, scroll_width = m$sw, client_width = m$cw,
                                       page_scrolls_horizontally = m$sw > m$cw + 1)
  rects <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('main table, main figure')).map(e => {const r = e.getBoundingClientRect(); return {x: r.left + scrollX, y: r.top + scrollY, w: r.width, h: r.height, tag: e.tagName.toLowerCase()}}))")$result$value)
  if (length(rects)) for (i in seq_len(nrow(rects))) {
    shot(file.path("qa", sprintf("%s_%s_%d_%s.png", p, w, i, rects$tag[i])),
         max(0, rects$x[i] - 5), max(0, rects$y[i] - 5), min(widths[[w]], rects$w[i] + 10), rects$h[i] + 10)
  }
}
b$close()
out <- do.call(rbind, res)
write.csv(out, "qa/overflow.csv", row.names = FALSE)
print(out)

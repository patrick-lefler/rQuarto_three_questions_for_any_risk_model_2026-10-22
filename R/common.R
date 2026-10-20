# Shared helpers for the four pages: palette, plot theme, formatters.
# Pages read cached artifacts in data/ only; nothing is refit here.
library(ggplot2)

okabe_ito <- c(blue = "#0072B2", orange = "#E69F00", green = "#009E73",
               vermillion = "#D55E00", sky = "#56B4E9", purple = "#CC79A7",
               grey = "#6B6B6B")

theme_series <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom",
          axis.text = element_text(colour = "#333333"),
          strip.text = element_text(face = "bold"))
}
theme_set(theme_series())
options(knitr.kable.NA = "—")

pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
num <- function(x, digits = 2) formatC(x, digits = digits, format = "f")
pval <- function(p) if (p < 0.001) "< 0.001" else sprintf("%.3f", p)

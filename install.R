# Installs all package dependencies (uses pak).
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pkg_install(c("probcal", "forecastdom", "falsifyr", "xgboost", "arrow",
                   "sandwich", "ggplot2", "dplyr", "tidyr", "testthat",
                   "knitr", "rmarkdown", "reactable"))

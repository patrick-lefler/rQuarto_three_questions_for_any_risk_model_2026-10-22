# Analysis windows (declared once; used by every analysis script).
# The simulate.R `split` column (train = months 1-30) is left untouched.
# Values come from params.yaml: classifier_train = VendorScore training,
# calibration_fit = overlay fitted (out of sample), overlay_eval = page 2,
# forecast_eval = page 3 raw-score comparison.
source("R/params.R")
WINDOWS <- lapply(par_get("windows"), function(w) w$value[1]:w$value[2])

window_counts <- function(d) {
  do.call(rbind, lapply(names(WINDOWS), function(w) {
    x <- d[d$month %in% WINDOWS[[w]], ]
    data.frame(window = w, months = paste(range(WINDOWS[[w]]), collapse = "-"),
               vendor_months = nrow(x), vendors = length(unique(x$vendor_id)),
               incidents = sum(x$incident))
  }))
}

# Causal rolling benchmark: forecast for month t is the mean of actual counts in
# months t-12..t-1 (NA for t = 1). Outcomes at month t or later never enter.
rolling_benchmark <- function(actual, k = par_get("forecast", "benchmark_months")) {
  vapply(seq_along(actual), function(t)
    if (t == 1) NA_real_ else mean(actual[max(1, t - k):(t - 1)]), numeric(1))
}

# Vendor-cluster resample: returns row indices carrying WHOLE vendor histories;
# a vendor drawn twice appears twice as separate copies (distinct draw labels).
cluster_resample_idx <- function(rows_by_vendor, vids = names(rows_by_vendor)) {
  drawn <- sample(vids, replace = TRUE)
  list(idx = unlist(rows_by_vendor[drawn], use.names = FALSE),
       draw = rep(seq_along(drawn), lengths(rows_by_vendor[drawn])))
}

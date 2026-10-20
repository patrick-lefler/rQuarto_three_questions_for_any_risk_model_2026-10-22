# Canonical bank incident-association baseline, shared by every script so that
# one definition feeds the headline, the specification curve, the deletion
# search and the scorecard. Mirrors the validated check in R/simulate.R:
#   tier: categorical, levels 3, 2, 1; data_access: categorical;
#   no_soc2 = 1 - soc2_current; log_patch = log(patch_latency / 20);
#   prior_c = pmin(prior_incidents_12m, 3); plus fin_health.

BASELINE_ID <- "bank_baseline_v2"
bank_formula <- incident ~ no_soc2 + tier + data_access + log_patch + prior_c + fin_health

prep_bank <- function(panel) {
  panel$no_soc2     <- 1 - panel$soc2_current
  panel$tier        <- factor(panel$tier, levels = c(3, 2, 1))
  panel$data_access <- factor(panel$data_access, levels = c("none", "internal", "client_pii"))
  panel$log_patch   <- log(panel$patch_latency / 20)
  panel$prior_c     <- pmin(panel$prior_incidents_12m, 3)
  panel
}

# Vendor-clustered inference: sandwich::vcovCL(type = "HC1", cadjust = TRUE),
# Wald z (normal reference), 95% confidence level.
CLUSTER_VCOV <- "sandwich::vcovCL(type = 'HC1', cadjust = TRUE); Wald z; 95% CI"

term_clustered <- function(fit, cluster, term = "no_soc2") {
  vc <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1", cadjust = TRUE)
  b <- unname(coef(fit)[term]); se <- unname(sqrt(diag(vc))[term])
  data.frame(coef = b, se = se, or = exp(b), low = exp(b - 1.96 * se),
             high = exp(b + 1.96 * se), p = 2 * pnorm(-abs(b / se)),
             row.names = NULL)
}

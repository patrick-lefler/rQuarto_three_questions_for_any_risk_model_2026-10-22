# Page 4 computations: how small a change removes the "no SOC 2 -> ~2x odds" finding?
# Pre-registered kill: the no_soc2 coefficient loses significance at the 5% level
# under vendor-clustered standard errors (R/baseline.R: CLUSTER_VCOV).
# Run from the project root after fit_models.R. Writes data/fragility.rds,
# data/specification_manifest.csv, data/seed_sweep.csv, data/deletion_path.csv.
# The seed sweep re-runs R/simulate.R in temporary folders (never touches data/).

library(sandwich)
source("R/baseline.R")
source("R/params.R")
set.seed(par_get("seed"))
ALPHA <- par_get("fragility", "alpha")

panel <- read.csv("data/vendor_panel.csv", stringsAsFactors = FALSE)
dat   <- prep_bank(panel)
m     <- readRDS("data/models.rds")
base_fit <- m$bank_glm
stopifnot(isTRUE(all.equal(coef(base_fit), coef(glm(bank_formula, binomial, dat)))))
base <- term_clustered(base_fit, dat$vendor_id)

# ---- Specification grid: 16 logit models, all on the odds-ratio scale ------------
# Fixed terms: no_soc2, tier, data_access, log_patch. Varied: vendor_size, prior_c,
# fin_health (include / omit) and one pre-specified trimming rule (drop vendor-months
# with patch_latency above the panel's 99th percentile). falsifyr::refit_model()
# performs each refit (replacement formula and data); clustered inference follows.
specs <- expand.grid(vendor_size = c(FALSE, TRUE), prior = c(TRUE, FALSE),
                     fin_health = c(TRUE, FALSE), trim = c(FALSE, TRUE))
cut_hi <- unname(quantile(dat$patch_latency, par_get("fragility", "trim_percentile")))
specs$spec_id <- sprintf("S%02d", seq_len(nrow(specs)))
curve <- do.call(rbind, lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i, ]
  rhs <- c("no_soc2", "tier", "data_access", "log_patch",
           if (s$prior) "prior_c", if (s$fin_health) "fin_health",
           if (s$vendor_size) "vendor_size")
  f <- reformulate(rhs, "incident")
  dd <- if (s$trim) dat[dat$patch_latency <= cut_hi, ] else dat
  fit <- falsifyr::refit_model(base_fit, data = dd, formula = f)
  stopifnot(identical(family(fit)$link, "logit"))
  r <- term_clustered(fit, dd$vendor_id)
  data.frame(spec_id = s$spec_id, formula = paste(deparse(f), collapse = ""),
             family = "binomial", link = "logit",
             filter_rule = if (s$trim) sprintf("patch_latency <= %.1f (panel 99th percentile)", cut_hi) else "none",
             n_rows = nrow(dd), n_vendors = length(unique(dd$vendor_id)),
             n_incidents = sum(dd$incident), vendor_size = s$vendor_size,
             prior = s$prior, fin_health = s$fin_health, trim = s$trim,
             coefficient = r$coef, effect_type = "odds ratio (logit)",
             se_method = CLUSTER_VCOV, or = r$or, low = r$low, high = r$high, p = r$p)
}))
curve$significant <- curve$p < ALPHA
curve <- curve[order(curve$or), ]; curve$rank <- seq_len(nrow(curve))
write.csv(curve[, setdiff(names(curve), c("rank"))], "data/specification_manifest.csv", row.names = FALSE)

plus_size <- curve[curve$vendor_size & curve$prior & curve$fin_health & !curve$trim, ]
stopifnot(nrow(plus_size) == 1)
ps <- data.frame(or = plus_size$or, low = plus_size$low, high = plus_size$high, p = plus_size$p)

# ---- Greedy deletion search (adversarial sensitivity stress test) -----------------
# Rule: delete rows from the ANALYSIS dataset (source incident history and the
# already-computed lagged prior_c are left as originally recorded). Candidates:
# incident rows at vendors without a current SOC 2 report (eligible supportive
# incidents). Each step shortlists the 15 candidates with the largest dfbeta for
# no_soc2, refits each, and deletes the one that raises the clustered p-value most.
# Budget: 80 deletions; stop when p >= 0.05. Post-search p-values are diagnostic.
BUDGET <- par_get("fragility", "deletion_budget"); N_CAND <- par_get("fragility", "deletion_shortlist")
dat$row_id <- seq_len(nrow(dat))
greedy_delete <- function() {
  keep <- rep(TRUE, nrow(dat)); fails <- 0L
  path <- data.frame(step = 0, removed_row = NA_integer_, vendor_id = NA_character_,
                     month = NA_integer_, p = base$p, or = base$or)
  for (k in seq_len(BUDGET)) {
    cur <- dat[keep, ]
    fit <- glm(bank_formula, family = binomial, data = cur)
    db <- dfbeta(fit)[, "no_soc2"]
    cand <- which(cur$incident == 1 & cur$no_soc2 == 1)
    cand <- cand[order(db[cand], decreasing = TRUE)][seq_len(min(N_CAND, length(cand)))]
    trial <- vapply(cand, function(i) {
      r <- tryCatch(term_clustered(glm(bank_formula, family = binomial, data = cur[-i, ]),
                                   cur$vendor_id[-i])$p, error = function(e) NA_real_)
      if (is.na(r)) fails <<- fails + 1L
      r
    }, numeric(1))
    best <- cand[which.max(trial)]
    drop_id <- cur$row_id[best]
    keep[drop_id] <- FALSE
    r <- term_clustered(glm(bank_formula, family = binomial, data = dat[keep, ]), dat$vendor_id[keep])
    path[nrow(path) + 1, ] <- list(k, drop_id, dat$vendor_id[drop_id], dat$month[drop_id], r$p, r$or)
    if (r$p >= ALPHA) break
  }
  attr(path, "failures") <- fails
  path
}
deletion_path <- greedy_delete()
write.csv(deletion_path, "data/deletion_path.csv", row.names = FALSE)
killed <- any(deletion_path$p >= ALPHA)
kill_rows <- if (killed) min(deletion_path$step[deletion_path$p >= ALPHA]) else NA
deletion_shares <- c(of_rows = kill_rows / nrow(dat),
                     of_incidents = kill_rows / sum(dat$incident),
                     of_eligible = kill_rows / sum(dat$incident == 1 & dat$no_soc2 == 1))
n_eligible <- sum(dat$incident == 1 & dat$no_soc2 == 1)

# ---- falsifyr within-model diagnostic (supplementary; not the clustered test) ----
FAL_ARGS <- list(term = "no_soc2", cluster = "vendor_id", profile = "default",
                 intensity = par_get("fragility", "falsifyr_intensity"), alpha = ALPHA,
                 kill_rule = "p_over_alpha", seed = par_get("seed"))
fal <- do.call(falsifyr::attack, c(list(base_fit, data = dat), FAL_ARGS, list(verbose = FALSE)))
fal_tab <- as.data.frame(fal$attacks)
list_cols <- vapply(fal_tab, is.list, logical(1))
fal_tab[list_cols] <- lapply(fal_tab[list_cols], function(col)
  vapply(col, function(v) paste(format(v), collapse = "; "), character(1)))
write.csv(fal_tab, "data/falsifyr_attacks.csv", row.names = FALSE)

# ---- Seed sweep: Monte Carlo repeatability of the synthetic generator ---------------
script <- normalizePath("R/simulate.R", mustWork = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
f_adj <- update(bank_formula, . ~ . + vendor_size)
one_seed <- function(sd) {
  dir <- tempfile("sim_"); dir.create(file.path(dir, "data"), recursive = TRUE)
  old <- setwd(dir); on.exit(setwd(old), add = TRUE)
  res <- tryCatch({
    system2(rscript, script, stdout = FALSE, stderr = FALSE, env = paste0("SIM_SEED=", sd))
    p <- prep_bank(read.csv("data/vendor_panel.csv"))
    a <- term_clustered(glm(bank_formula, binomial, p), p$vendor_id)
    b <- term_clustered(glm(f_adj, binomial, p), p$vendor_id)
    data.frame(seed = sd, status = "ok", incidents = sum(p$incident),
               naive_or = a$or, naive_low = a$low, naive_high = a$high, naive_p = a$p,
               adj_or = b$or, adj_low = b$low, adj_high = b$high, adj_p = b$p)
  }, error = function(e) data.frame(seed = sd, status = conditionMessage(e), incidents = NA,
                                    naive_or = NA, naive_low = NA, naive_high = NA, naive_p = NA,
                                    adj_or = NA, adj_low = NA, adj_high = NA, adj_p = NA))
  res
}
seed_or <- do.call(rbind, lapply(1:40, one_seed))
write.csv(seed_or, "data/seed_sweep.csv", row.names = FALSE)

saveRDS(list(curve = curve, base = base, plus_size = ps,
             deletion_path = deletion_path, kill_rows = kill_rows, killed = killed,
             deletion_shares = deletion_shares, n_eligible = n_eligible,
             n_rows = nrow(dat), n_incidents = sum(dat$incident),
             budget = BUDGET, n_cand = N_CAND,
             deletion_failures = attr(deletion_path, "failures"),
             falsifyr = fal, falsifyr_args = FAL_ARGS,
             falsifyr_version = as.character(packageVersion("falsifyr")),
             seed_or = seed_or, true_or = 1.25, trim_cut = cut_hi,
             simulate_md5 = unname(tools::md5sum(script)), baseline_id = BASELINE_ID,
             cluster_vcov = CLUSTER_VCOV),
        "data/fragility.rds")
cat("analysis_fragility.R done\n")

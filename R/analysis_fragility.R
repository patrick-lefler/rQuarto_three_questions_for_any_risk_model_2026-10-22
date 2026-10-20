# Page 4 computations: how small a change kills the "no SOC 2 -> ~2x odds" finding?
# Pre-registered kill: the no_soc2 coefficient loses significance at the 5% level
# under vendor-clustered standard errors.
# Run from the project root after fit_models.R. Writes data/fragility.rds.
# The seed sweep re-runs R/simulate.R in temporary folders (never touches data/).

library(sandwich)
set.seed(34)
ALPHA <- 0.05

m <- readRDS("data/models.rds")
dat <- m$bank_data
dat$vendor_size <- read.csv("data/vendor_panel.csv")$vendor_size

fit_term <- function(formula, data, family = binomial("logit"), term = "no_soc2") {
  fit <- glm(formula, family = family, data = data)
  se <- sqrt(diag(vcovCL(fit, cluster = data$vendor_id)))[term]
  b <- coef(fit)[term]
  data.frame(or = exp(b), low = exp(b - 1.96 * se), high = exp(b + 1.96 * se),
             p = 2 * pnorm(-abs(b / se)), row.names = NULL)
}

# ---- Specification curve ------------------------------------------------------
specs <- expand.grid(vendor_size = c(FALSE, TRUE), prior = c(TRUE, FALSE),
                     fin_health = c(TRUE, FALSE), link = c("logit", "cloglog"),
                     trim = c(FALSE, TRUE), stringsAsFactors = FALSE)
cut_hi <- quantile(dat$patch_latency, 0.99)
curve <- do.call(rbind, lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i, ]
  rhs <- c("no_soc2", "tier", "data_access", "log_patch",
           if (s$prior) "prior_c", if (s$fin_health) "fin_health",
           if (s$vendor_size) "vendor_size")
  dd <- if (s$trim) dat[dat$patch_latency <= cut_hi, ] else dat
  cbind(s, fit_term(reformulate(rhs, "incident"), dd, binomial(s$link)))
}))
curve$significant <- curve$p < ALPHA
curve <- curve[order(curve$or), ]
curve$rank <- seq_len(nrow(curve))

# ---- Smallest kill -----------------------------------------------------------
base_formula <- formula(m$bank_glm)
base <- fit_term(base_formula, dat)
plus_size <- fit_term(update(base_formula, . ~ . + vendor_size), dat)

# Greedy deletion of incident rows among vendors without SOC 2; at each step
# test the 15 highest-influence candidates and drop the one that raises p most.
greedy_delete <- function(max_steps = 80, n_cand = 15) {
  keep <- rep(TRUE, nrow(dat)); path <- data.frame(removed = 0, p = base$p, or = base$or)
  for (k in seq_len(max_steps)) {
    cur <- dat[keep, ]
    fit <- glm(base_formula, family = binomial, data = cur)
    db <- dfbeta(fit)[, "no_soc2"]
    cand <- which(cur$incident == 1 & cur$no_soc2 == 1)
    cand <- cand[order(db[cand], decreasing = TRUE)][seq_len(min(n_cand, length(cand)))]
    trial <- vapply(cand, function(i) {
      fit_term(base_formula, cur[-i, ])$p
    }, numeric(1))
    best <- cand[which.max(trial)]
    keep[which(keep)[best]] <- FALSE
    r <- fit_term(base_formula, dat[keep, ])
    path[nrow(path) + 1, ] <- c(k, r$p, r$or)
    if (r$p >= ALPHA) break
  }
  path
}
deletion_path <- greedy_delete()
kill_rows <- if (any(deletion_path$p >= ALPHA)) min(deletion_path$removed[deletion_path$p >= ALPHA]) else NA
n_inc_nosoc2 <- sum(dat$incident == 1 & dat$no_soc2 == 1)

# ---- falsifyr within-model attacks (not clustered; see data/package_check.txt)
fal <- falsifyr::attack(m$bank_glm, term = "no_soc2", data = dat, cluster = "vendor_id",
                        seed = 34, intensity = "normal", verbose = FALSE)

# ---- Seed sensitivity: re-run simulate.R for seeds 1-40 -----------------------
script <- normalizePath("R/simulate.R", mustWork = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
f_naive <- base_formula; f_full <- update(base_formula, . ~ . + vendor_size)
seed_or <- do.call(rbind, lapply(1:40, function(sd) {
  dir <- tempfile("sim_"); dir.create(file.path(dir, "data"), recursive = TRUE)
  old <- setwd(dir); on.exit(setwd(old), add = TRUE)
  system2(rscript, script, stdout = FALSE, stderr = FALSE, env = paste0("SIM_SEED=", sd))
  p <- read.csv("data/vendor_panel.csv")
  p$no_soc2 <- 1L - p$soc2_current; p$log_patch <- log(p$patch_latency)
  p$prior_c <- p$prior_incidents_12m - mean(p$prior_incidents_12m)
  p$tier <- factor(p$tier)
  p$data_access <- factor(p$data_access, levels = c("none", "internal", "client_pii"))
  setwd(old)
  a <- fit_term(f_naive, p); b <- fit_term(f_full, p)
  data.frame(seed = sd, incidents = sum(p$incident), naive_or = a$or, naive_p = a$p,
             adj_or = b$or, adj_p = b$p)
}))

saveRDS(list(curve = curve, base = base, plus_size = plus_size,
             deletion_path = deletion_path, kill_rows = kill_rows,
             n_inc_nosoc2 = n_inc_nosoc2, falsifyr = fal, seed_or = seed_or,
             true_or = 1.25, trim_cut = cut_hi),
        "data/fragility.rds")
cat("analysis_fragility.R done\n")

# Review response: revision log

One row per ticket. "Confirmed" means verified in the code or artifacts before changing
anything. All old numbers were recomputed; none of the revised results were tuned to
recover earlier findings. `R/simulate.R` is unchanged (md5 in `data/analysis_manifest.csv`);
its 34 tests pass.

## Evidence log (before changes)

| Item | Status | Evidence |
|---|---|---|
| Calibrator fitted on classifier-training scores | **Confirmed** | Old `analysis_calibration.R` fitted the overlay on months 1-30, the rows VendorScore trained on (`fit_models.R`). |
| Baseline 2.06 vs validated 2.04 | **Confirmed** | `fit_models.R` used a mean-centred, uncapped `prior_incidents_12m`; `simulate.R` uses `pmin(prior, 3)`. The `tier` coding and `log(patch/20)` offset only move the reference level and intercept. |
| Specification curve mixed links | **Confirmed** | Draft grid had logit and cloglog; both were labelled "odds ratio". |
| Forecast feature leakage | **Not found** | `prior_incidents_12m` covers months t-12..t-1 only. `patch_latency` and `fin_health` are drawn with month t; treated as start-of-month observations (stated assumption). |
| falsifyr "cannot add a variable" | **Overstated** | `refit_model(model, data, formula)` is documented and accepts a replacement formula. |

## Tickets

| Ticket | Action | Evidence / tests | Limitation |
|---|---|---|---|
| P0-A windows | Classifier 1-30; overlay fitted 31-36; overlay assessed 37-48 (3,600 vendor-months, 75 incidents). Page 3 keeps raw scores on 31-48 and says so. Methods (isotonic primary, Platt on logit scale secondary, equal-size ECE primary) fixed before viewing 37-48. | `R/windows.R`; script asserts disjoint windows; `tests/test_fit_models.R` "windows are disjoint"; window counts in `data/analysis_manifest.csv`. | Only 28 incidents in the fitting window. Platt looks better on point estimates but was not selected after the fact. Retrospective demonstration, not prospective. |
| P0-B overlay recommendation | Replaced with "do not approve the tested overlay for routine use yet..." Tier chart relabelled "risk bands", described as retrospective illustration; bands use explicit boundaries (Low <1%, Medium >=1% and <3%, High >=3%); distinct from criticality tier. Scorecard has separate result and response cells. | `fragility.qmd` scorecard; `calibration.qmd`. | No operational acceptance threshold was set; the page says so and does not invent one. |
| P0-C specification curve | 16 logit specifications (vendor size x prior incidents x financial health x one trimming rule). Dashed line relabelled "data-generating conditional OR". | `data/specification_manifest.csv` (formula, family, link, filter, n, coefficient, SE method, CI, p); test "specification curve is logit odds ratios". | Trimming rule is one pre-specified choice (99th percentile of patch latency). |
| P0-D forecast information set | Origin defined as start of month t; benchmark = mean of actuals t-12..t-1; feature-availability table in manifest; raw scores only on page 3. | `rolling_benchmark()` tests (hand-worked example; changing outcomes from month 20 on does not change earlier forecasts). | Start-of-month availability of patch latency / financial health is an assumption of the synthetic design. |
| P1-A baseline | One canonical formula and preprocessing (`R/baseline.R`) used by fit, curve, deletion search, seed sweep and scorecard. Baseline OR 2.04 (1.48-2.82 clustered). | Test "one baseline feeds every page" (headline = S01 = deletion path start = seed 34 sweep row). | - |
| P1-B dependence | Calibration intervals and metric differences resample whole vendor histories (1,000 draws), raw and overlay paired; conditional on the fitted overlay. Kernel test moved to a folded exploratory note with its independence limitation. | `cluster_resample_idx()` test (whole histories, 12 months each). | Intervals exclude overlay-fitting uncertainty. Reliability bins fixed from the point sample. |
| P1-C calibration explanations | Brier = overall probability error; ECE = binned gap, bin-dependent; isotonic described as interpolated monotone curve; Platt input scale recorded; drift explanation downgraded to a hypothesis; bin-specific true mean shown instead of a global reference line; Brier changes in ordinary units; ties not split (5 distinct isotonic groups). | `data/calibration.rds` (`ece_def`, `calibrators`). | - |
| P1-D forecast | Units fixed (MSE incidents-squared, MAE incidents); p-value wording corrected; `h`, loss, alternative, error order and correction stated; cumulative advantage defined; fee claim replaced; exploratory AUC paragraph removed. | `forecastdom::dm_test` source inspected: at `h = 1` variance is sample variance / n (no autocorrelation adjustment); DM sign/symmetry test; cumulative advantage test. | Observation, not tested: the package's one-sided p-value convention appears inverted relative to its documentation. We use only the two-sided test. |
| P1-E fragility wording | "Smallest kill" replaced by "successful perturbation within the search budget"; shares reported (0.20% of rows, 13.9% of incidents, 20.6% of eligible); deletion defined as removal from the analysis dataset (lagged history kept); path, rule, budget and failure count saved. | `data/deletion_path.csv`; `fragility.rds` (budget 80, shortlist 15, failures 0). | Greedy search: upper bound only. Post-search p-values are diagnostics. |
| P1-F falsifyr | `refit_model()` performs each specification refit; `attack()` call, version and statuses recorded; limitation narrowed to what was verified. | `data/falsifyr_attacks.csv`; footnote on page 4. | Not verified whether `attack()`'s bounded specification search considers variables outside the fitted formula. |
| P1-G seed sensitivity | Presented as a Monte Carlo repeatability check; seed 34 selection disclosed; sweep recomputed with the final baseline and clustered inference. | `data/seed_sweep.csv` (all 40 seeds, status column); generator md5 in manifest. | Counts are descriptive. |
| P1-H regulatory framing | Materiality box rewritten; asset-threshold phrasing corrected; black-box sentence corrected; framework attributed to this project; guidance, package and repository links added. | Date, agencies, $30 billion wording and non-prescriptive language confirmed against OCC Bulletin 2026-13. `tests/check_links.R`: 163 links, 0 failing. | Section citations checked against the guidance PDF text (April 17, 2026). Corrected: benchmarking is in §V (conceptual soundness), alternative-assumption testing is in §IV, and "an overlay must show fitness" was our judgement, not the guidance's. Added: §V "established performance thresholds", §VI documentation of exceptions, §II asset-manager scope. | - |
| P2 presentation | "Hook" renamed; `NA` cells replaced by a dash; real footnotes; reader notes shortened (two short notes per object kept); short sidebar title, author, date, synthetic label; previous/next, source and Seeking Eureka links; zero-preserving square-root reliability axes; zero-width caps. | `tests/test_pages.R`; `qa/` screenshots; `qa/overflow.csv` (no horizontal page scroll at 1280 and 390 px). | Mobile tables scroll inside their own container. Screenshots reviewed for layout only, not a full accessibility audit. |
| Word gate | Rendered-HTML count including reader notes, captions, callouts, footnotes; 3,742 words total. | `R/word_count.R`, `data/word_count.csv`. | - |

## Addendum: fifth page

A short fifth page, Reproducibility and Limits (`reproducibility.qmd`), was added at the
owner's request, overriding the brief's "no fifth page" guidance. It carries one table
(main settings and their basis), the limitations list and a one-command run. Every analysis
parameter now lives in `params.yaml` (value, basis, sensitivity range) and is read by the
scripts; outputs were verified unchanged after the refactor (same score checksum and
metrics). Test counts on the page come from `data/test_results.csv`. Rendered word count
is 3,971 of the 4,000 ceiling, so the four analysis pages were tightened to make room.

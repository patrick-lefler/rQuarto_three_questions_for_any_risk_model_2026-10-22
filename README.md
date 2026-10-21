# Three Questions for Any Risk Model

A five-page Quarto website, a Seeking Eureka research series on model risk. A fictional
bank, NorthPoint, tests a black-box vendor model (VendorScore) with three questions:
calibration (`probcal`), forecast dominance (`forecastdom`), fragility (`falsifyr`).
All data are synthetic; every result is reproducible from seed 34. Author: Patrick Lefler.

## Reproduce

Run from the project root. The environment is locked with `renv` (`renv.lock`).

```sh
Rscript run_all.R                    # everything below, in order, then render and tests
```

or step by step:

```sh
Rscript install.R                    # package dependencies (pak)
Rscript R/simulate.R                 # synthetic panel -> data/
Rscript R/fit_models.R               # VendorScore scores + bank model -> data/
Rscript R/check_packages.R           # Gate 0 -> data/package_check.txt
Rscript R/analysis_calibration.R     # data/calibration.rds
Rscript R/analysis_forecast.R        # data/forecast.rds
Rscript R/analysis_fragility.R       # data/fragility.rds + CSVs (~2 min: 40-seed sweep)
Rscript R/build_manifest.R           # manifest, checksums, session info
Rscript tests/run_tests.R            # test counts -> data/test_results.csv
quarto render                        # -> docs/
Rscript R/word_count.R               # rendered-word count (ceiling 4,100)
```

Pages read cached artifacts in `data/` only; they never refit a model.

## Analysis design (corrected after review; see `REVIEW_RESPONSE.md`)

| Window | Months | Role |
|---|---|---|
| Classifier training | 1-30 | VendorScore (xgboost) trained here |
| Calibration fit | 31-36 | recalibration overlay fitted here |
| Overlay evaluation | 37-48 | page 2 results; nothing fitted or selected here |
| Forecast evaluation | 31-48 | page 3, raw scores vs 12-month average |

The bank's baseline model (`R/baseline.R`) mirrors the validated check in `R/simulate.R`
and uses vendor-clustered standard errors throughout.

## Tests and QA

```sh
Rscript tests/run_tests.R            # runs the two suites below, records data/test_results.csv
Rscript tests/test_simulate.R        # simulation (34 checks)
Rscript tests/test_fit_models.R      # artifacts, windows, baseline, benchmark, DM, resampling
Rscript tests/test_pages.R           # render, word budget, figure/table limits, reader notes
Rscript tests/check_links.R          # internal and external links -> qa/link_check.csv
Rscript tests/qa_screenshots.R       # desktop/mobile screenshots -> qa/
```

## Audit artifacts (`data/`)

`analysis_manifest.csv` (windows, models, score variants, feature availability),
`specification_manifest.csv` (all 16 curve estimates), `seed_sweep.csv`, `deletion_path.csv`,
`falsifyr_attacks.csv`, `package_check.txt`, `checksums.csv`, `session_info.txt`,
`test_results.csv`, `word_count.csv`. Every analysis parameter is registered in `params.yaml`
(read by the scripts, with basis and sensitivity ranges).

## Notes

- `vendorscore.parquet` / `.csv` hold scores only; no page accesses the fitted model.
- The planted overconfidence sharpens logits around the training base rate (the original
  0.5 pivot failed its own test; see `R/fit_models.R`).
- `falsifyr` supplies the specification refits and a supplementary attack run; the
  vendor-clustered kill test and deletion search are custom code (`R/analysis_fragility.R`).
- Reader notes under each figure and table count toward the word budget.
- The guidance is non-prescriptive; all recommendations read "sound practice suggests…".

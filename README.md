# Trust, but Verify: Validating a Vendor Risk Model You Can't See Inside

A four-page Quarto website (a Seeking Eureka research series on model risk). A fictional
bank, NorthPoint, tests a black-box vendor model (VendorScore) with three questions:
calibration (`probcal`), forecast dominance (`forecastdom`), fragility (`falsifyr`).
All data are synthetic; every result is reproducible from seed 34.

## Reproduce

Run from the project root:

```sh
Rscript install.R                    # package dependencies (pak)
Rscript R/simulate.R                 # synthetic panel -> data/
Rscript R/fit_models.R               # VendorScore scores + bank model -> data/
Rscript R/check_packages.R           # Gate 0 -> data/package_check.txt
Rscript R/analysis_calibration.R     # data/calibration.rds
Rscript R/analysis_forecast.R        # data/forecast.rds
Rscript R/analysis_fragility.R       # data/fragility.rds (~2 min: seeds 1-40 sweep)
Rscript R/word_count.R               # prose budget (ceiling 4,000 words)
quarto render                        # -> docs/
```

Pages read cached artifacts in `data/` only; they never refit a model.

## Tests

```sh
Rscript tests/test_simulate.R        # simulation (34 checks)
Rscript tests/test_fit_models.R      # artifact contracts
Rscript tests/test_pages.R           # render, word budget, figure/table limits
```

## Notes

- `vendorscore.parquet` / `.csv` hold scores only; no page accesses the fitted model.
- Deviation from the spec: the planted overconfidence sharpens logits around the training
  base rate instead of 0.5 (see comments in `R/fit_models.R`).
- `falsifyr` runs, but cannot use vendor-clustered standard errors or add an omitted
  variable, so the pre-registered kill is a hand-built loop (see `data/package_check.txt`).
- Reader notes under each figure and table sit outside the word budget (`.reader-note`).
- The guidance is non-prescriptive; all recommendations read "sound practice suggests…".

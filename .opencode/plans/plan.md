# PatagoniaMet: Full Python Refactor Plan

> **Last updated**: 2026-05-25
> **Status**: Phase 0 (scaffolding) complete. Phases 1–9 not started.
> **R files remaining**: 26 of 26 (none deleted)
> **Python modules ported**: 0 of 26
> **Tests**: 0

## Problem
The library is currently a mix of R scripts and Jupyter notebooks.
The goal is to refactor everything into a well-structured Python package,
deleting R files as each is ported.

## Approach
- Restructure as a proper Python data science package
- Use `xarray`, `rioxarray`, `geopandas`, `pandas`, `numpy` to replace `terra` + `zoo`
- Use `scikit-learn` to replace `caret` (Random Forest / RFE)
- Use `ruptures` to replace `changepoint.np`
- Use `hydroeval` or custom code to replace `hydroGOF`
- Mark `reddPrec` (precipitation QC) and `qmap` (quantile mapping) as TODO stubs
- Manage dependencies with Poetry (`pyproject.toml`)
- Keep Jupyter notebooks minimal — for exploration and running scripts
- Delete R files as each module is ported
- Use `pytest` for unit/integration tests
- Use `ruff` for linting and `mypy` for type checking
- All functions must include type hints and NumPy-style docstrings
- Use `logging` module (not print) for pipeline progress

---

## Target Package Structure

```
PatagoniaMet/
├── pyproject.toml            # Poetry config + dependencies + scripts
├── README.md
├── config.yml                # keep as-is, but paths resolved via pathlib
├── .gitignore                # updated for Python artifacts
├── .editorconfig             # consistent formatting
├── patagoniaMet/             # main Python package
│   ├── __init__.py
│   ├── config.py             # config loader with pathlib resolution
│   ├── core/                 # shared primitives
│   │   ├── __init__.py
│   │   ├── resample.py       # ← src/TimeResample.R
│   │   ├── changepoint.py    # ruptures wrapper
│   │   └── types.py          # shared type aliases
│   ├── processors/           # data transformation pipelines
│   │   ├── __init__.py
│   │   ├── qc/               # quality checks
│   │   │   ├── pp.py         # ← quality_check/Data_Consistency_PP.R
│   │   │   ├── t2m.py        # ← quality_check/Data_Consistency_T2M.R
│   │   │   ├── q.py          # ← quality_check/Data_Consistency_Q.R
│   │   │   └── pet.py        # ← quality_check/Data_Consistency_PET.R
│   │   ├── bias_correction/  # bias correction + downscaling
│   │   │   ├── pp.py         # ← bias_correction/Bias_Correction_PP1.R + PP2.R
│   │   │   ├── t2m.py        # ← bias_correction/Bias_Correction_T2M.R
│   │   │   └── downscaling.py# ← bias_correction/Downscaling_PP_T2M.R
│   │   └── spatial/          # basins, covariables, GIS
│   │       ├── basins.py     # ← processing/Basins_Attributes.R
│   │       └── covariables.py# ← processing/Covariables_RF.R
│   ├── models/               # scientific models
│   │   ├── __init__.py
│   │   ├── tuw/              # TUW hydrological model
│   │   │   ├── input.py      # ← evaluation/TUWmodel_input.R
│   │   │   ├── calibration.py# ← evaluation/TUWmodel_calibration.R
│   │   │   └── output.py     # ← evaluation/TUWmodel_output.R
│   │   └── ml/               # ML wrappers
│   │       ├── rf.py         # RandomForest + RFECV (scikit-learn)
│   │       └── rfe.py        # recursive feature elimination
│   ├── metrics/              # evaluation metrics
│   │   ├── __init__.py
│   │   └── hydrology.py      # KGE, NSE, RMSE (replaces hydroGOF)
│   └── plotting/             # figure generation
│       ├── __init__.py
│       ├── styles.py         # shared matplotlib/seaborn styles
│       ├── basin_attr.py     # ← figures/Figure_basin_attr.R
│       ├── comparison.py     # ← figures/Figure_comparison.R
│       ├── figure5.py        # ← figures/Figure5.R
│       ├── figure6.py        # ← figures/Figure6.R + Figure6_slides.R
│       ├── figureS1.py       # ← figures/FigureS1.R
│       ├── figureS2.py       # ← figures/FigureS2.R
│       ├── figureS3.py       # ← figures/FigureS3.R
│       └── figureS4.py       # ← figures/FigureS4.R
├── notebooks/                # lightweight exploration notebooks
│   ├── 01_processing.ipynb   # ← processing/Basins_delimitation.ipynb, Data_extent.ipynb
│   ├── 02_quality_check.ipynb
│   ├── 03_bias_correction.ipynb
│   ├── 04_evaluation.ipynb
│   └── 05_postprocessing.ipynb # ← processing/Postprocessing.ipynb, Potential_Evaporation.ipynb
├── tests/                    # pytest test suite
│   ├── test_core/
│   ├── test_processors/
│   ├── test_models/
│   └── test_metrics/
├── data/                     # keep (add to .gitignore if >100MB)
├── dataset/                  # keep
├── results/                  # keep
├── figures/                  # keep (R originals deleted after porting)
└── reports/                  # keep as-is (Word docs, not code)
```

---

## R → Python Library Mapping

| R Package         | Python Equivalent             | Notes                         |
|-------------------|-------------------------------|-------------------------------|
| `zoo`             | `pandas`                      | time series, rolling windows  |
| `terra` (raster)  | `xarray` + `rioxarray`        | raster I/O, extract, focal    |
| `terra` (vector)  | `geopandas`                   | SpatVector → GeoDataFrame     |
| `caret` (RF/RFE)  | `scikit-learn`                | RandomForest, RFECV            |
| `hydroGOF`        | `hydroeval` or custom         | KGE, NSE, RMSE                |
| `doMC`            | `joblib` / `concurrent.futures` | parallelism                 |
| `changepoint.np`  | `ruptures`                    | PELT changepoint detection    |
| `qmap`            | **TODO stub**                 | quantile mapping (PTF)        |
| `reddPrec`        | **TODO stub**                 | precipitation QC flags        |

---

## Code Standards

- **Type hints**: All function signatures must include type hints
- **Docstrings**: NumPy-style docstrings for all public functions
- **Logging**: Use `logging` module with module-level loggers (no `print`)
- **Config**: Load via `config.yml` using `pydantic-settings` or `omegaconf` with `pathlib.Path` resolution
- **Raster data**: `xarray.DataArray` with CF-compliant time coordinates
- **Testing**: `pytest` with fixtures for sample data; aim for >70% coverage on core utils
- **Linting**: `ruff` for linting + formatting
- **Type checking**: `mypy` with strict mode
- **Pre-commit**: `pre-commit` hook running `ruff` + `mypy` + `pytest`

---

## Todos (ordered by dependency)

### Phase 0 — Package scaffolding ✅
- [x] `scaffold`: Create `pyproject.toml` (Poetry), package folder `patagoniaMet/`, `__init__.py` files
- [x] `tooling`: Add `ruff`, `mypy`, `pytest`, `pre-commit` configuration
- [x] `gitignore`: Update `.gitignore` for Python artifacts (`.pyc`, `__pycache__`, `.pytest_cache`, etc.)
- [x] `config-loader`: Create `patagoniaMet/config.py` to load `config.yml` with pathlib path resolution
- [ ] `dep-fix`: Add missing dependencies to `pyproject.toml` — `ruptures`, `hydroeval`, `seaborn`
- [ ] `editorconfig`: Create `.editorconfig` for consistent formatting

### Phase 1 — Core utilities
- [ ] `core-resample`: Port `src/TimeResample.R` → `patagoniaMet/core/resample.py`
  - `monthly_resample(daily_data, days_min, func)` using pandas GroupBy
  - `annual_resample(monthly_data, months_min, func)`
- [ ] `core-changepoint`: Create `patagoniaMet/core/changepoint.py` (ruptures wrapper)
- [ ] `core-types`: Create `patagoniaMet/core/types.py` (shared type aliases)
- [ ] `test-resample`: Add unit tests for resample functions

### Phase 2 — Spatial processing (needed by Bias Correction)
- [ ] `proc-basins`: Port `Basins_Attributes.R` → `patagoniaMet/processors/spatial/basins.py`
- [ ] `proc-cov`: Port `Covariables_RF.R` → `patagoniaMet/processors/spatial/covariables.py`
  - RF/RFE → scikit-learn

### Phase 3 — Quality Check
- [ ] `qc-pp`: Port `quality_check/Data_Consistency_PP.R` → `patagoniaMet/processors/qc/pp.py`
  - reddPrec call → TODO stub with comment
- [ ] `qc-t2m`: Port `quality_check/Data_Consistency_T2M.R` → `patagoniaMet/processors/qc/t2m.py`
- [ ] `qc-q`: Port `quality_check/Data_Consistency_Q.R` → `patagoniaMet/processors/qc/q.py`
- [ ] `qc-pet`: Port `quality_check/Data_Consistency_PET.R` → `patagoniaMet/processors/qc/pet.py`

### Phase 4 — Bias Correction
- [ ] `bc-pp`: Port `Bias_Correction_PP1.R` + `Bias_Correction_PP2.R` → `patagoniaMet/processors/bias_correction/pp.py`
  - qmap PTF → TODO stub
  - RF/RFE → scikit-learn
- [ ] `bc-t2m`: Port `Bias_Correction_T2M.R` → `patagoniaMet/processors/bias_correction/t2m.py`
- [ ] `bc-downscaling`: Port `Downscaling_PP_T2M.R` → `patagoniaMet/processors/bias_correction/downscaling.py`

### Phase 5 — Models
- [ ] `metrics`: Create `patagoniaMet/metrics/hydrology.py` (KGE, NSE, RMSE)
- [ ] `ml-rf`: Create `patagoniaMet/models/ml/rf.py` (RandomForest + RFECV wrapper)
- [ ] `ml-rfe`: Create `patagoniaMet/models/ml/rfe.py` (recursive feature elimination)
- [ ] `tuw-input`: Port `TUWmodel_input.R` → `patagoniaMet/models/tuw/input.py`
- [ ] `tuw-calibration`: Port `TUWmodel_calibration.R` → `patagoniaMet/models/tuw/calibration.py`
- [ ] `tuw-output`: Port `TUWmodel_output.R` → `patagoniaMet/models/tuw/output.py`

### Phase 6 — Evaluation (uses metrics + model outputs)
- [ ] `eval-pp`: Port `Validation_PP.R` → uses `patagoniaMet/metrics/` + notebook
- [ ] `eval-t2m`: Port `Validation_T2M.R` → uses `patagoniaMet/metrics/` + notebook
- [ ] `eval-pet`: Port `Validation_PET.R` → uses `patagoniaMet/metrics/` + notebook

### Phase 7 — Figures
- [ ] `fig-styles`: Create `patagoniaMet/plotting/styles.py` (shared matplotlib/seaborn styles)
- [ ] `fig-basin-attr`: Port `Figure_basin_attr.R` → `patagoniaMet/plotting/basin_attr.py`
- [ ] `fig-comparison`: Port `Figure_comparison.R` → `patagoniaMet/plotting/comparison.py`
- [ ] `fig-5`: Port `Figure5.R` → `patagoniaMet/plotting/figure5.py`
- [ ] `fig-6`: Port `Figure6.R` + `Figure6_slides.R` → `patagoniaMet/plotting/figure6.py`
- [ ] `fig-S1`: Port `FigureS1.R` → `patagoniaMet/plotting/figureS1.py`
- [ ] `fig-S2`: Port `FigureS2.R` → `patagoniaMet/plotting/figureS2.py`
- [ ] `fig-S3`: Port `FigureS3.R` → `patagoniaMet/plotting/figureS3.py`
- [ ] `fig-S4`: Port `FigureS4.R` → `patagoniaMet/plotting/figureS4.py`

### Phase 8 — Notebooks
- [ ] `notebooks`: Port existing notebooks and create minimal exploration notebooks for each phase
- [ ] `notebook-01`: Processing (Basins_delimitation + Data_extent)
- [ ] `notebook-02`: Quality check
- [ ] `notebook-03`: Bias correction
- [ ] `notebook-04`: Evaluation
- [ ] `notebook-05`: Postprocessing + Potential Evaporation

### Phase 9 — Cleanup
- [ ] `delete-r`: Delete all ported R files
- [ ] `delete-r-figures`: Delete all ported R figure scripts
- [ ] `verify`: Run full pipeline end-to-end and compare outputs with R results

---

## Key Decisions
- `reddPrec` and `qmap` stubs will raise `NotImplementedError` with docstrings explaining the R logic
- Config is loaded from `config.yml` using `pydantic-settings` or `omegaconf`
- Paths in config.yml will be resolved via `pathlib.Path` relative to project root or via environment variables
- Raster time series use `xarray.DataArray` with CF-compliant time coordinates
- TUW model is split into 3 modules (input, calibration, output) matching original R file separation
- Figures use `matplotlib`/`seaborn` with a shared `plotting/styles.py` module
- `reports/` directory is kept as-is (contains Word documents, not code)
- Large data files (>100MB) should be excluded from git; consider DVC if versioning is needed
- No dedicated `io/` module — data loading uses standard `xarray.open_dataset()`, `pandas.read_csv()`, `geopandas.read_file()` inline in each module

## Migration Strategy
- During transition, R and Python code will coexist
- Each module is ported, tested, and verified against R output before deleting the R original
- Notebooks will call Python modules (not R scripts) once ported
- Final verification: run full pipeline in Python and compare outputs with saved R results

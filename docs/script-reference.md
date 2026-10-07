# Script reference

| Order | Script | Function | Principal inputs | Principal outputs |
|---|---|---|---|---|
| 00 | `scripts/00_check_environment.R` | Verifies R packages, protected inputs, parameters, and output directories before analysis. | `.Renviron`, `renv.lock` | Console validation report |
| 01 | `scripts/01_build_ab_databases.R` | Selects the current regional-data branch and runs the A/B constructor. | Eight regional workbooks | Six A/B aggregate workbooks and audit comparison |
| 01 implementation | `R/pipeline/build_ab_databases.R` | Imports and harmonises regions, derives variables, applies the qualifying-colonoscopy window, randomises FIT assignment, validates counts, and exports A/B datasets. | Regional and historical protected inputs | Derived A/B datasets |
| 02 | `scripts/02_build_descriptive_table.R` | Creates the independent descriptive table for participants without family history. | Eight regional workbooks | Descriptive TSV, LaTeX, and validation files |
| 03 | `scripts/03_estimate_local_fit_sensitivity.R` | Runs the patient-level random-FIT sensitivity and specificity diagnostic. | Transformed high-risk PRENEC cohort | Diagnostic workbook |
| 03 implementation | `R/pipeline/estimate_local_fit_sensitivity.R` | Defines random FIT selection and lesion-specific Wilson estimates. | Transformed high-risk cohort | In-memory tables consumed by script 03 |
| 04 | `scripts/04_build_external_fit_parameters.R` | Reproduces the four-study random-effects sensitivity meta-analysis for low-risk/non-advanced adenoma. | Published TP/FN counts embedded and documented in the script | Meta-analysis audit files |
| 05 | `scripts/05_estimate_adherence_qc.R` | Independently checks colonoscopy participation by A/B, sex, and age. | Six A/B workbooks | Participation QC tables |
| 06 | `scripts/06_run_publication_analysis.R` | Runs the final Monte Carlo model for adjusted detection and prevalence. | A/B datasets, high-risk cohort, FIT parameters, colonoscopy parameters | Publication TSV and RDS outputs |
| 07 | `scripts/07_validate_publication_analysis.R` | Validates output structure and numerical invariants. | Script 06 result object | Reproducibility-check table |
| 08 | `scripts/08_make_publication_figure.R` | Creates the final multipanel Lancet-style figure. | Both-sex estimates from script 06 | PDF, PNG, TIFF, SVG, and plotted data |
| all | `scripts/run_all.R` | Executes the ordered workflow in separate R sessions and stops on the first failure. | Configured project | Complete local analysis |

The two parameter readers in `R/pipeline/` validate the schemas and active
records in the literature-derived workbooks. They are retained as auditable
utilities even though script 06 contains its own strict readers.


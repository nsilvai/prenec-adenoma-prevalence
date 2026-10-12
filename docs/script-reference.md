# Script reference

| Order | Script | Function | Principal inputs | Principal outputs |
|---|---|---|---|---|
| 00 | `scripts/00_check_environment.R` | Verifies R packages, protected inputs, parameters, and output directories before analysis. | `.Renviron`, `renv.lock` | Console validation report |
| 01 | `scripts/01_build_ab_databases.R` | Selects the current regional-data branch and runs the A/B constructor using the corrected denominator of participants with at least one observed FIT. | Eight restricted regional workbooks | Six A/B aggregate workbooks and audit comparison |
| 01 implementation | `R/pipeline/build_ab_databases.R` | Imports and harmonises regions, derives variables, applies the qualifying-colonoscopy window, creates complementary FIT assignments, limits `N` to participants with at least one observed FIT, validates counts, and exports A/B datasets. | Restricted regional and historical inputs | Derived A/B datasets |
| 02 | `scripts/02_build_descriptive_table.R` | Creates the independent descriptive table for participants without reported family history. | Eight restricted regional workbooks | Descriptive TSV, LaTeX, and validation files |
| 03 | `scripts/03_estimate_local_fit_sensitivity.R` | Runs the participant-level random-FIT sensitivity and specificity diagnostic. | Restricted transformed high-risk PRENEC cohort | Local diagnostic workbook |
| 03 implementation | `R/pipeline/estimate_local_fit_sensitivity.R` | Defines random FIT selection and lesion-specific Wilson estimates. | Restricted transformed high-risk cohort | In-memory tables consumed by Stage 03 |
| 04 | `scripts/04_build_external_fit_parameters.R` | Reproduces the four-study random-effects sensitivity meta-analysis for low-risk/non-advanced adenoma. | Published TP/FN counts embedded and documented in the script | Meta-analysis audit files |
| 05 | `scripts/05_estimate_adherence_qc.R` | Independently checks colonoscopy participation by A/B assignment, sex, and age. | Six A/B workbooks | Participation QC tables |
| 06 | `scripts/06_run_publication_analysis.R` | Runs the earlier aggregate Monte Carlo implementation retained for reproducibility and comparison. | A/B datasets, restricted high-risk cohort, FIT parameters, colonoscopy parameters | Aggregate-analysis TSV and RDS outputs |
| 07 | `scripts/07_validate_publication_analysis.R` | Validates Stage 06 output structure and numerical invariants. | Stage 06 result object | Reproducibility-check table |
| 08 | `scripts/08_make_publication_figure.R` | Creates the aggregate-analysis multipanel figure. | Both-sex estimates from Stage 06 | PDF, PNG, TIFF, SVG, and plotted data |
| 09 | `scripts/09_prepare_joint_bootstrap_frame.R` | Builds the corrected analytical cohort, creates a restricted deidentified participant frame, and reconciles it exactly to the six released A/B aggregates. | Eight restricted regional workbooks and six released A/B aggregates | Restricted Git-ignored participant RDS and Git-ignored non-identifying reconciliation manifest |
| 10 | `scripts/10_run_joint_bootstrap_uncertainty.R` | Runs the primary sex-by-age joint participant bootstrap, adds separate sensitivity draws, produces four uncertainty runs, and performs exact additive log-scale variance decomposition. | Restricted Stage 09 frame, restricted high-risk cohort, FIT parameters, colonoscopy parameters | Git-ignored primary TSV and RDS outputs under `results/joint_bootstrap/` |
| 11 | `scripts/11_compare_joint_to_current.R` | Strictly crosswalks the primary full-analysis results to the earlier aggregate implementation. | Stage 06 estimates and Stage 10 run summaries | Git-ignored 96-row comparison of points, intervals, differences, and interval-width ratios |
| 12 | `scripts/12_make_joint_bootstrap_figures.R` | Creates primary full-analysis figures while preserving the FIT-scenario and A/B visual encoding. | Stage 10 run summaries | Working PDF, PNG, TIFF, SVG, plot data, and captions under ignored `results/`; disclosure-reviewed PDF and aggregate plot-data TSV copies are tracked under `manuscript/` |
| 13 | `scripts/13_make_main_variance_share_table.R` | Creates the Base A, both-sex variance-share table for four lesion endpoints, two estimands, and both FIT-sensitivity scenarios. | Stage 10 log-scale variance shares | Git-ignored publication-ready TSV, Markdown, and LaTeX tables, plus a detailed long TSV |
| 14 | `scripts/14_make_variance_decomposition_figure.R` | Creates the stacked-bar variance-source figure for Base A and both sexes. The current figure displays low-risk adenoma, high-risk adenoma, and colorectal cancer for detection and prevalence; the underlying Stage 10 output retains all four endpoints. | Stage 10 log-scale variance shares | Working PDF, PNG, TIFF, SVG, and plotted-data TSV under ignored `results/`; disclosure-reviewed PDF and aggregate plot-data TSV copies are tracked under `manuscript/` |
| all | `scripts/run_all.R` | Runs the aggregate Stages 06--08 in separate R sessions, optionally rebuilding intermediates or adding QC checks; this helper does not run the primary participant bootstrap. | Configured project, released A/B products, and protected sensitivity inputs | Complete local aggregate comparison analysis |

The primary result set is produced by Stage 10 and reported by Stages 12--14.
Low-risk adenoma, high-risk adenoma, any adenoma, and colorectal cancer are
carried through the primary estimates and the variance-share table. Stage 14
uses the selected three-endpoint display stated above.

The two parameter readers in `R/pipeline/` validate the schemas and active
records in the literature-derived workbooks. They remain auditable utilities,
including for the aggregate comparison implementation.

Stages 09--14 require authorised participant-level data access and must be run
inside approved secure storage. The Stage 09 frame remains restricted even
after direct identifiers are removed. Participant frames, bootstrap draws, full
result tables and RDS objects, manifests, and other working outputs remain
excluded from version control. The only exception is the disclosure-reviewed
publication set: five PDFs under `manuscript/figures/` and three aggregate
plot-data TSVs under `manuscript/source_data/` are tracked. Scripts 12 and 14
continue to write their working outputs to the ignored results directory; only
approved copies are placed under `manuscript/`.

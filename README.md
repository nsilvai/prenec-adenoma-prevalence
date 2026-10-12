# PRENEC lesion detection and prevalence model

This repository contains the reproducible R workflow used to estimate
patient-level lesion detection under universal colonoscopy and prevalence in
PRENEC. The primary analysis uses a joint participant bootstrap and reports four
endpoints: low-risk adenoma, high-risk adenoma, any adenoma, and colorectal
cancer. Estimates are produced for FIT assignments A and B, both sexes together
and men and women separately, and PRENEC-derived and literature-based FIT
sensitivity scenarios.

The analytical denominator is restricted to average-risk participants aged
50--75 years who returned at least one FIT. Participants who never returned a
FIT cannot enter the observed FIT-to-colonoscopy detection pathway and are not
included in `N`. In the current data this gives `N = 31,104` (`8,267` men and
`22,837` women).

## Confidentiality

PRENEC individual-level data are restricted health data and are **not included**.
The repository contains only the vetted A/B products aggregated by age and sex,
which report population and detected-lesion counts without participant
identifiers or individual dates. The deidentified participant frame required by
the primary bootstrap analysis remains restricted because it contains one row
per participant.

Stages 09--14 require authorised access to participant-level PRENEC data and
must be run only in approved secure storage. The participant frame, bootstrap
draws, full result tables and RDS objects, manifests, and other working outputs
remain excluded from version control. The narrow exception is the
disclosure-reviewed publication set: five approved PDFs are tracked under
`manuscript/figures/`, and three aggregate plot-data TSVs are tracked under
`manuscript/source_data/`. The source data include direct or indirect
identifiers and must never be copied into the repository.

See [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md) for the access statement and
[docs/analysis-decisions.md](docs/analysis-decisions.md) for the documented
data-source boundaries.

## Analysis workflow

```text
Eight restricted regional workbooks
        |
        +--> 01 Build corrected A/B aggregates
        |         |-- denominator: at least one observed FIT
        |         |-- both sexes, men, and women
        |         `-- four lesion endpoints
        |
        +--> 02 Descriptive programme table
        |
        `--> 09 Build and reconcile restricted participant frame
                  |
                  `--> 10 Primary joint participant-bootstrap analysis
                            |-- lesion detection under universal colonoscopy
                            |-- prevalence
                            |-- four uncertainty runs
                            `-- exact log-scale variance decomposition
                                  |
                                  +--> 12 publication figures
                                  +--> 13 variance-share table
                                  `--> 14 variance-decomposition figure

Stage 06 aggregate Monte Carlo analysis + Stage 10 primary results
        |
        `--> 11 one-to-one methodological comparison
```

Stages 06--08 retain the aggregate Monte Carlo implementation as a reproducible
comparison pathway. They are not the primary uncertainty analysis after the
joint participant bootstrap was adopted.

A detailed diagram and the exact input/output contract are provided in
[docs/pipeline.md](docs/pipeline.md). The role of every executable file is
listed in [docs/script-reference.md](docs/script-reference.md).

## Reproducible environment

The project targets R 4.4.2 and uses `renv` to record package versions.

1. Open `PRENEC-prevalence.Rproj`.
2. Run `renv::restore()`.
3. Copy `.Renviron.example` to `.Renviron`.
4. Replace the placeholder paths with locations inside approved secure storage.
5. Run the environment check:

```r
Rscript scripts/00_check_environment.R
```

The aggregate helper runs the comparison pathway from the released A/B products
and the configured protected sensitivity inputs:

```r
Rscript scripts/run_all.R
```

Optional diagnostic estimates can be included with:

```r
Rscript scripts/run_all.R --with-qc
```

Authorised users who have configured all eight restricted regional workbooks
and the protected historical comparison files may reconstruct the corrected A/B
products before running the aggregate pathway:

```r
Rscript scripts/run_all.R --rebuild-intermediate
```

The primary participant-bootstrap workflow must be run in an authorised data
environment. Stage 09 must finish successfully before Stage 10, and Stage 10
must precede Stages 11--14. Stage 11 also requires the Stage 06 comparison
results.

```r
Rscript scripts/09_prepare_joint_bootstrap_frame.R
Rscript scripts/10_run_joint_bootstrap_uncertainty.R
Rscript scripts/11_compare_joint_to_current.R
Rscript scripts/12_make_joint_bootstrap_figures.R
Rscript scripts/13_make_main_variance_share_table.R
Rscript scripts/14_make_variance_decomposition_figure.R
```

Stage 09 writes a deidentified participant-level analysis frame to the path
configured by `PRENEC_BOOTSTRAP_FRAME`. Although direct identifiers are removed,
this file remains restricted health data and is Git-ignored. Stages 10--14 write
their working outputs under `results/joint_bootstrap/`; that directory remains
ignored. Scripts 12 and 14 therefore write working figures and plot data only to
the ignored results directory. After disclosure review, approved publication
copies are placed under `manuscript/figures/` and
`manuscript/source_data/`, where the five PDFs and three aggregate TSVs noted
above are tracked. No bootstrap draws, full result tables or RDS files, or
manifests are included in those publication directories.

Exact reproduction of the PRENEC-derived FIT-sensitivity scenario additionally
requires authorised access to the transformed high-risk PRENEC cohort configured
as `PRENEC_BASE_SENS`; this restricted file is not distributed. The six vetted
A/B intermediate workbooks under `data/intermediate/` are the only tracked
PRENEC-derived intermediate datasets. The only additional tracked analysis
outputs are the narrowly defined, disclosure-reviewed publication assets under
`manuscript/`.

## Core estimands

Within sex-by-age stratum `h`, let `N_h` be participants in the corrected
analytical denominator, `F^+_h` those with a positive selected FIT, `C_h` those
FIT-positive participants completing a qualifying colonoscopy, and `D_jh` those
with lesion `j` detected histologically. The empirical pathway component is

```text
Q_j = sum_h (N_h/N) (F^+_h/N_h) (D_jh/C_h).
```

This is algebraically equivalent to correcting observed detection by
colonoscopy participation within each stratum, while retaining the nesting of
the counts in the bootstrap. Lesion detection under universal colonoscopy and
prevalence are then estimated as

```text
r_j = Q_j / Se_FIT,j
p_j = Q_j / (Se_FIT,j * Se_COL,j).
```

The same resampled participants are used across A/B assignments, lesion
endpoints, populations, and FIT-sensitivity scenarios. FIT- and
colonoscopy-sensitivity uncertainty is added separately.

## Repository status

The code and non-confidential parameter inputs are prepared for open access.
Raw and row-level PRENEC data remain restricted. Working primary-analysis
outputs remain untracked; only the disclosure-reviewed publication PDFs and
aggregate plot-data TSVs under `manuscript/` are tracked.

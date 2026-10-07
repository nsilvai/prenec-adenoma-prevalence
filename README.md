# PRENEC lesion detection and prevalence model

This repository contains the reproducible R workflow used to estimate
patient-level adjusted population detection and modelled prevalence of
low-risk adenoma, high-risk adenoma, and colorectal cancer in PRENEC.

The code preserves the analytical definitions used for the manuscript. The
repository curation changes documentation, file organisation, and path
configuration only; it does not intentionally change the statistical analysis.

## Confidentiality

PRENEC individual-level data are restricted health data and are **not included**.
The repository contains only the intermediate A/B products aggregated by age
and sex, which report population and detected-lesion counts without participant
identifiers or individual dates. Simulation objects and all other generated
results remain excluded. The source data include direct or indirect identifiers
and must remain in approved secure storage.

See [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md) for the access statement and
[docs/analysis-decisions.md](docs/analysis-decisions.md) for the documented
data-source boundaries.

## Analysis workflow

```text
Eight restricted regional workbooks
        |
        +--> 01 Build complementary A/B datasets
        |         |-- both sexes
        |         |-- men
        |         `-- women
        |
        `--> 02 Descriptive programme table

A/B datasets + restricted high-risk cohort + literature parameters
        |
        `--> 06 Monte Carlo analysis
                  |-- adjusted population detection
                  `-- modelled prevalence
                          |
                          +--> 07 validation
                          `--> 08 publication figure
```

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

6. Run the publication pipeline from the released A/B intermediate products:

```r
Rscript scripts/run_all.R
```

Optional diagnostic estimates can be included with:

```r
Rscript scripts/run_all.R --with-qc
```

Authorised users who have configured all eight restricted regional workbooks
and the protected historical comparison files may reconstruct the A/B products
before running the downstream analysis:

```r
Rscript scripts/run_all.R --rebuild-intermediate
```

The source-data processing code is therefore fully visible, while the source
workbooks themselves remain outside the repository. Exact downstream
reproduction of the local FIT-sensitivity scenario also requires authorised
access to the transformed high-risk PRENEC cohort configured as
`PRENEC_BASE_SENS`; this restricted file is not distributed.

The six vetted A/B intermediate workbooks under `data/intermediate/` are tracked.
All other generated data and results remain untracked by design.

## Core estimands

For lesion category `j`, the model uses the observed population detection rate
`d_j = D_j/N`. It estimates adjusted population detection by correcting for FIT
sensitivity and colonoscopy participation conditional on a positive selected
FIT, and estimates modelled prevalence by additionally correcting for
colonoscopy sensitivity. All lesion outcomes are defined at the patient level.

## Repository status

The repository is prepared for open access. Raw PRENEC data remain restricted;
only the documented age-by-sex A/B intermediate products are released.


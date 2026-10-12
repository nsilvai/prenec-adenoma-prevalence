# Analysis and repository decisions

## Primary analysis

The current primary uncertainty analysis is the joint participant bootstrap in
Stages 09--14. The earlier aggregate Monte Carlo implementation in Stages
06--08 is retained for reproducibility and for the one-to-one Stage 11
comparison, but it no longer defines the primary uncertainty intervals.

The joint analysis resamples complete observed participant patterns within
sex-by-age strata. A single bootstrap bank is reused across A/B assignments,
lesion endpoints, reported populations, and FIT-sensitivity scenarios. This
preserves the empirical dependence of FIT positivity, qualifying colonoscopy,
and histologically confirmed lesion detection instead of drawing nested counts
independently.

## Corrected analytical denominator

The analytical denominator is average-risk PRENEC participants aged 50--75
years with at least one observed FIT. Participants without any observed FIT are
excluded because they could not enter the observed FIT-positive and
colonoscopy-confirmed detection pathway. The current participant frame contains
`31,104` people: `8,267` men and `22,837` women.

The same denominator definition is used in the corrected A/B aggregates and
the participant frame. Stage 09 must reproduce every released age-specific A/B
count exactly before it saves the frame. A participant with one observed FIT
contributes that observed result to both assignments; participants with two
results retain the reproducible complementary A/B assignment.

## Estimands and lesion endpoints

For sex-by-age stratum `h`, `N_h` is the analytical population, `F^+_h` is the
number with a positive selected FIT, `C_h` is the number of FIT-positive
participants completing a qualifying colonoscopy, and `D_jh` is the number in
`C_h` with lesion category `j` detected histologically. The empirical pathway
component is

```text
Q_j = sum_h (N_h/N) (F^+_h/N_h) (D_jh/C_h).
```

This equals the observed detection rate corrected by colonoscopy participation
within each stratum. Expressing it through FIT positivity and colonoscopy yield
does not remove the participation correction; it makes the nested count
structure explicit for joint resampling.

The two estimands are

```text
lesion detection under universal colonoscopy: r_j = Q_j / Se_FIT,j
prevalence:                              p_j = Q_j / (Se_FIT,j * Se_COL,j)
```

The primary result set carries four patient-level, histologically ascertained
endpoints: low-risk adenoma, high-risk adenoma, any adenoma, and colorectal
cancer. Results are produced for assignments A/B, both sexes/men/women, and
PRENEC-derived/literature-based FIT sensitivity. Stage 12 and Stage 13 report
all four endpoints. The current Stage 14 stacked-bar figure displays low-risk
adenoma, high-risk adenoma, and colorectal cancer; the any-adenoma decomposition
remains available in the Stage 10 output and Stage 13 table.

## Uncertainty analysis

Stage 10 separates three input blocks:

- screening-data uncertainty from the stratified participant bootstrap;
- FIT-sensitivity uncertainty; and
- colonoscopy-sensitivity uncertainty.

It exports four runs: each input block varied separately, followed by the full
analysis in which all applicable sources vary. Point estimates are observed-
count and point-sensitivity plug-in estimates; the simulation is used for
equal-tail percentile 95% intervals.

Variance shares use the exact additive log-scale decomposition implied by the
independence assumption. For lesion detection under universal colonoscopy, the
total attributable variance is the sum of the log variances of `Q_j` and FIT
sensitivity. For prevalence, colonoscopy-sensitivity log variance is added.
Each share is its component divided by this sum, so shares sum to one by
construction. The empirical full-draw log variance is retained as a Monte
Carlo check rather than substituted for the additive denominator.

## Current data-source boundary

The target-population A/B datasets and the Stage 09 participant frame are
reconstructed from the eight current regional workbooks. Local FIT sensitivity
is estimated from the transformed historical `Base_prenec.xlsx` high-risk
cohort. This is an intentional description of the executable pipeline, not a
claim that the two sources are interchangeable. Any future unification of these
sources requires a separate equivalence analysis and must not be introduced as
a documentation-only change.

The standalone FIT-sensitivity and colonoscopy-participation scripts provide
auditable diagnostic estimates. Primary Stage 10 reads the restricted
sensitivity cohort and parameter workbooks directly; diagnostic exports are
not substituted for those inputs.

## Confidentiality decisions

- No row-level PRENEC data are versioned.
- Stages 09--14 require authorised participant-level data access and may be run
  only in approved secure storage.
- The Stage 09 deidentified frame remains restricted health data and is
  Git-ignored; removing direct identifiers does not make it a public artifact.
- The six A/B intermediate workbooks are released because the project requires
  the detected-lesion counts by age and sex to accompany the open code. They
  contain no identifiers or individual dates. Some cells contain small counts;
  this remains explicitly documented for governance review.
- Participant frames, bootstrap draws, full result tables and RDS objects,
  manifests, and other Stage 09--14 working outputs remain untracked. Scripts
  12 and 14 write working figures and plot data under ignored `results/` paths.
- The narrow publication exception consists of five disclosure-reviewed PDFs
  tracked under `manuscript/figures/` and three aggregate plot-data TSVs tracked
  under `manuscript/source_data/`. These are approved copies; the underlying
  working formats and complete result objects remain ignored.
- Literature-derived parameter workbooks are included because they contain no
  participant-level PRENEC records.
- Third-party article PDFs are not redistributed.

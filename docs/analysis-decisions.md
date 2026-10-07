# Analysis and repository decisions

## Preserved analytical definitions

Repository preparation did not intentionally alter the lesion definitions,
FIT threshold, qualifying-colonoscopy window, A/B randomisation, denominators,
Monte Carlo distributions, seeds, or model equations. Changes are limited to
documentation, safe path configuration, entrypoint organisation, and the
reproducible software environment.

## Current data-source boundary

The target-population A/B datasets are reconstructed from the eight current
regional workbooks. Local FIT sensitivity in the final model is estimated from
the transformed historical `Base_prenec.xlsx` high-risk cohort. This is an
intentional description of the current executable pipeline, not a claim that
the two sources are interchangeable. Any future unification of these sources
requires a separate equivalence analysis and must not be introduced as a
documentation-only change.

## Diagnostic scripts versus model inputs

The standalone FIT-sensitivity and colonoscopy-participation scripts provide
auditable diagnostic estimates. The publication model currently re-estimates
both quantities internally rather than reading those diagnostic exports. This
duplication is documented because it creates a maintenance risk. Modularising
to one shared implementation should be performed only with exact regression
tests against the archived publication results.

## Confidentiality decisions

- No row-level PRENEC data are versioned.
- The six A/B intermediate workbooks are released because the project requires
  the detected-lesion counts by age and sex to accompany the open code. They
  contain no identifiers or individual dates. Some cells contain small counts;
  this is explicitly documented for governance review.
- Result objects other than the specified A/B intermediates remain untracked.
- Literature-derived parameter workbooks are included because they contain no
  participant-level PRENEC records.
- Third-party article PDFs are not redistributed.


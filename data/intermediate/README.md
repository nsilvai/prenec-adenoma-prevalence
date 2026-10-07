# Released A/B intermediate products

This directory contains the direct outputs of the script that processes the
restricted regional PRENEC workbooks. The six files represent complementary
FIT assignments A and B for both sexes, men, and women.

Each row is an age. No participant code, national identifier, centre, date, FIT
measurement, colonoscopy record, or free-text clinical field is included.

## Variables

| Variable | Definition |
|---|---|
| `Edad_2` | Age at programme enrolment |
| `pop` | Number of eligible participants at that age |
| `fit_observed_n` | Participants with an observed selected FIT |
| `fit_positive_n` | Participants whose selected FIT was at least 100 ng Hb/mL |
| `colonoscopy_n` | FIT-positive participants with a qualifying colonoscopy |
| `adenoma_total` | Participants with at least one detected adenoma |
| `ad_small`, `ad_medium`, `ad_large` | Participants with a detected adenoma in the recorded size category |
| `low_risk`, `high_risk` | Participants with at least one lesion in the specified risk category |
| `cancer` | Participants with detected colorectal cancer |
| `ads1`--`adl3m` | Size-by-recorded-number diagnostic counts retained from the original A/B output |
| `adenoma_sin_clasificacion_tamano` | Adenoma count not reconciled to a recorded size category |
| `adenoma_sin_clasificacion_riesgo` | Adenoma count not reconciled to low- or high-risk classification |

The publication analysis restricts these files to ages 50--75. Values outside
that range are retained because these are the unmodified intermediate outputs
of the database-construction script.

`SHA256SUMS.txt` records the cryptographic checksum of each released workbook.
The checksums were compared with the corresponding files in the protected
analysis workspace before publication; all six copies matched byte for byte.


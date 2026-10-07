# Data availability

The individual-level PRENEC data used in this study are not distributed with
the repository. They contain health information and direct or indirect
identifiers, including participant codes, national identifiers, dates, centre,
sex, FIT measurements, colonoscopy dates, and pathological findings.

Access to the source data is subject to the governance, ethics, and information
security requirements of the participating institutions. Researchers wishing
to reproduce the analysis must obtain authorised access independently and map
their protected files to the environment variables documented in
`.Renviron.example`.

The literature-derived parameter workbooks included under `data/parameters/`
contain no participant-level PRENEC records. Six intermediate A/B workbooks are
available under `data/intermediate/`; they contain only age-specific population,
testing-pathway, and detected-lesion counts for both sexes, men, and women, with
no participant identifiers or individual dates. Simulation objects and all
other generated result tables are excluded from version control.


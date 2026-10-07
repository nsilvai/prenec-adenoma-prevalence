# Ordered PRENEC publication workflow -----------------------------------------
#
# Each stage runs in a separate R session to prevent objects or options from one
# script leaking into the next. By default, the workflow begins with the six
# released A/B workbooks. Use --rebuild-intermediate only in an authorised
# environment containing the restricted regional source data. Use --with-qc to
# add the three optional audit stages.

arguments <- commandArgs(trailingOnly = TRUE)
include_qc <- "--with-qc" %in% arguments
rebuild_intermediate <- "--rebuild-intermediate" %in% arguments
allowed_arguments <- c("--with-qc", "--rebuild-intermediate")
unknown_arguments <- setdiff(arguments, allowed_arguments)
if (length(unknown_arguments) > 0L) {
  stop(
    "Unknown argument(s): ", paste(unknown_arguments, collapse = ", "),
    call. = FALSE
  )
}

all_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub(
  "^--file=", "",
  grep("^--file=", all_arguments, value = TRUE)[[1]]
)
project_root <- normalizePath(
  file.path(dirname(script_file), ".."),
  winslash = "/",
  mustWork = TRUE
)
setwd(project_root)
Sys.setenv(PRENEC_PROJECT_ROOT = project_root)

analysis_steps <- c(
  "scripts/06_run_publication_analysis.R",
  "scripts/07_validate_publication_analysis.R",
  "scripts/08_make_publication_figure.R"
)
rebuild_steps <- c(
  "scripts/01_build_ab_databases.R",
  "scripts/02_build_descriptive_table.R"
)
optional_qc_steps <- c(
  "scripts/03_estimate_local_fit_sensitivity.R",
  "scripts/04_build_external_fit_parameters.R",
  "scripts/05_estimate_adherence_qc.R"
)

steps <- c(
  if (rebuild_intermediate) rebuild_steps else character(),
  if (include_qc) optional_qc_steps else character(),
  analysis_steps
)

rscript <- file.path(R.home("bin"), "Rscript")

check_scope <- if (rebuild_intermediate) "full" else "analysis"
message("\n=== Running scripts/00_check_environment.R ===")
check_status <- system2(
  rscript,
  c(
    shQuote(normalizePath("scripts/00_check_environment.R", winslash = "/")),
    paste0("--scope=", check_scope)
  )
)
if (!identical(check_status, 0L)) {
  stop(
    "Pipeline stopped because scripts/00_check_environment.R failed.",
    call. = FALSE
  )
}

for (step in steps) {
  message("\n=== Running ", step, " ===")
  status <- system2(
    rscript,
    shQuote(normalizePath(step, winslash = "/"))
  )
  if (!identical(status, 0L)) {
    stop("Pipeline stopped because ", step, " failed.", call. = FALSE)
  }
}

message("\nPRENEC publication pipeline completed successfully.")


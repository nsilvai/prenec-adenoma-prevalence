# PRENEC repository preflight check -------------------------------------------
# Purpose: fail before analysis if software or required inputs are missing.
# This script reads metadata only; it does not import participant records.

arguments <- commandArgs(trailingOnly = TRUE)
scope_argument <- grep("^--scope=", arguments, value = TRUE)
scope <- if (length(scope_argument) == 0L) {
  "analysis"
} else {
  sub("^--scope=", "", scope_argument[[1]])
}
if (!scope %in% c("analysis", "full")) {
  stop("--scope must be either analysis or full.", call. = FALSE)
}

script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub(
  "^--file=", "",
  grep("^--file=", script_arguments, value = TRUE)[[1]]
)
project_root <- normalizePath(
  file.path(dirname(script_file), ".."),
  winslash = "/",
  mustWork = TRUE
)
Sys.setenv(PRENEC_PROJECT_ROOT = project_root)

required_packages <- c(
  "coda", "dplyr", "ggplot2", "janitor", "lubridate", "meta",
  "patchwork", "purrr", "ragg", "readxl", "rlang", "stringr",
  "svglite", "tidyr", "writexl"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    ". Run renv::restore().",
    call. = FALSE
  )
}

required_environment <- "PRENEC_BASE_SENS"
if (identical(scope, "full")) {
  required_environment <- c(
    required_environment,
    "PRENEC_DATA_DIR",
    "PRENEC_BASE_A_ACTUAL",
    "PRENEC_BASE_B_ACTUAL"
  )
}
missing_environment <- required_environment[
  !nzchar(Sys.getenv(required_environment, unset = ""))
]
if (length(missing_environment) > 0L) {
  stop(
    "Missing .Renviron settings: ",
    paste(missing_environment, collapse = ", "),
    call. = FALSE
  )
}

private_names <- "PRENEC_BASE_SENS"
if (identical(scope, "full")) {
  private_names <- c(
    private_names,
    "PRENEC_BASE_A_ACTUAL",
    "PRENEC_BASE_B_ACTUAL"
  )
}
private_files <- Sys.getenv(private_names)
missing_private_files <- private_files[!file.exists(private_files)]
if (length(missing_private_files) > 0L) {
  stop(
    "Configured protected inputs do not exist: ",
    paste(basename(missing_private_files), collapse = ", "),
    call. = FALSE
  )
}

regional_files <- character()
if (identical(scope, "full")) {
  raw_directory <- normalizePath(
    Sys.getenv("PRENEC_DATA_DIR"),
    winslash = "/",
    mustWork = TRUE
  )
  regional_files <- list.files(
    raw_directory,
    pattern = "^[^~].*\\.xlsx$",
    full.names = TRUE
  )
  if (length(regional_files) != 8L) {
    stop(
      "Expected eight regional workbooks in PRENEC_DATA_DIR; found ",
      length(regional_files), ".",
      call. = FALSE
    )
  }
}

intermediate_files <- file.path(
  project_root,
  "data",
  "intermediate",
  c(
    "Base_finalA_N_aleatoria_cohorte_actual_semilla_20260923.xlsx",
    "Base_finalA_N_aleatoria_hombres_cohorte_actual_semilla_20260923.xlsx",
    "Base_finalA_N_aleatoria_mujeres_cohorte_actual_semilla_20260923.xlsx",
    "Base_finalB_N_aleatoria_cohorte_actual_semilla_20260923.xlsx",
    "Base_finalB_N_aleatoria_hombres_cohorte_actual_semilla_20260923.xlsx",
    "Base_finalB_N_aleatoria_mujeres_cohorte_actual_semilla_20260923.xlsx"
  )
)
if (identical(scope, "analysis") && any(!file.exists(intermediate_files))) {
  stop("One or more released A/B intermediate workbooks are missing.", call. = FALSE)
}

parameter_files <- file.path(
  project_root,
  "data",
  "parameters",
  c(
    "colonoscopy_sensitivity_parameters.xlsx",
    "external_fit_sensitivity_parameters.xlsx"
  )
)
if (any(!file.exists(parameter_files))) {
  stop("One or more literature parameter workbooks are missing.", call. = FALSE)
}

message("PRENEC environment check passed.")
message("Check scope: ", scope)
message("R version: ", R.version.string)
if (identical(scope, "full")) {
  message("Regional workbooks found: ", length(regional_files))
}
if (identical(scope, "analysis")) {
  message("Released A/B workbooks found: ", length(intermediate_files))
}
message("Project root: ", project_root)


# Project-path helpers ---------------------------------------------------------
#
# These functions keep personal or institutional storage locations outside the
# versioned code. PRENEC_PROJECT_ROOT is set automatically by the entrypoints;
# protected input locations are supplied through .Renviron.

prenec_script_file <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", arguments, value = TRUE)
  if (length(file_argument) == 0L) {
    return(NA_character_)
  }
  normalizePath(
    sub("^--file=", "", file_argument[[1]]),
    winslash = "/",
    mustWork = TRUE
  )
}

prenec_project_root <- function() {
  configured <- Sys.getenv("PRENEC_PROJECT_ROOT", unset = "")
  if (nzchar(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  }

  script <- prenec_script_file()
  candidates <- c(
    if (!is.na(script)) dirname(dirname(script)) else character(0),
    getwd()
  )
  candidates <- unique(normalizePath(
    candidates,
    winslash = "/",
    mustWork = FALSE
  ))
  marker <- file.path(candidates, "PRENEC-prevalence.Rproj")
  match <- candidates[file.exists(marker)]
  if (length(match) == 0L) {
    stop(
      "Project root not found. Set PRENEC_PROJECT_ROOT or run from the repository root.",
      call. = FALSE
    )
  }
  match[[1]]
}

prenec_required_env_path <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(name, " is not configured. See .Renviron.example.", call. = FALSE)
  }
  normalizePath(value, winslash = "/", mustWork = TRUE)
}


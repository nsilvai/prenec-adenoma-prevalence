# Stage 03: local FIT-sensitivity diagnostic -----------------------------------
#
# Purpose:
#   Estimate patient-level FIT sensitivity and specificity in the protected
#   high-risk cohort after randomly selecting one available FIT per participant.
# This diagnostic export is not read directly by the final publication model;
# the model repeats the documented calculation internally.

argumentos <- commandArgs(trailingOnly = FALSE)
archivo_argumento <- grep("^--file=", argumentos, value = TRUE)

if (length(archivo_argumento) > 0L) {
  archivo_actual <- normalizePath(
    sub("^--file=", "", archivo_argumento[[1]]),
    winslash = "/",
    mustWork = TRUE
  )
  directorio_script <- dirname(archivo_actual)
} else {
  directorio_script <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

directorio_proyecto <- normalizePath(
  file.path(directorio_script, ".."),
  winslash = "/",
  mustWork = TRUE
)
Sys.setenv(PRENEC_PROJECT_ROOT = directorio_proyecto)

locale_resultado <- suppressWarnings(
  Sys.setlocale("LC_CTYPE", "Spanish_Chile.utf8")
)
if (is.na(locale_resultado)) {
  message(
    "Spanish_Chile.utf8 is unavailable; UTF-8 source encoding will be used."
  )
}

source(
  file.path(
    directorio_proyecto,
    "R",
    "pipeline",
    "estimate_local_fit_sensitivity.R"
  ),
  encoding = "UTF-8",
  chdir = FALSE,
  echo = FALSE
)

ruta_salida <- Sys.getenv(
  "PRENEC_SENS_OUTPUT",
  unset = file.path(
    directorio_proyecto,
    "results",
    "quality_control",
    "local_fit_sensitivity.xlsx"
  )
)
dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)

writexl::write_xlsx(
  list(
    Sensitivity = result_table,
    Specificity = specificity_result_table,
    Specificity_no_neoplasia = specificity_no_neoplasia_table,
    Diagnostic_detail = result_table_diagnostic,
    Sensitivity_counts = detalle_sensibilidad,
    Specificity_counts = detalle_especificidad,
    Randomization = resumen_sorteo_sensibilidad,
    Sensitivity_comparison = comparacion_sensibilidad,
    Specificity_comparison = comparacion_especificidad
  ),
  ruta_salida
)

message("Resultados guardados en: ", normalizePath(ruta_salida, winslash = "/", mustWork = TRUE))

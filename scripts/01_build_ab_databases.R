# Stage 01: build the complementary A/B analysis databases --------------------
#
# Purpose:
#   Run the database-construction implementation on the eight current regional
#   PRENEC workbooks without changing its analytical definitions.
# Inputs:
#   PRENEC_DATA_DIR, PRENEC_BASE_A_ACTUAL, and PRENEC_BASE_B_ACTUAL.
# Outputs:
#   Six age-aggregated workbooks: A/B for both sexes, men, and women, plus
#   reproducibility audits. Outputs remain outside version control.
# Random component:
#   One FIT round is selected per participant using PRENEC_SEED. Assignment B is
#   complementary to A when both rounds are observed.

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

Sys.setenv(PRENEC_USAR_BASE_HISTORICA = "0")
if (!nzchar(Sys.getenv("PRENEC_OUTPUT_DIR", unset = ""))) {
  Sys.setenv(
    PRENEC_OUTPUT_DIR = file.path(
      directorio_proyecto,
      "data",
      "intermediate"
    )
  )
}

suppressMessages(
  source(
    file.path(
      directorio_proyecto,
      "R",
      "pipeline",
      "build_ab_databases.R"
    ),
    encoding = "UTF-8",
    chdir = FALSE,
    echo = FALSE
  )
)

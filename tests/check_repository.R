# Repository-level checks that require no restricted PRENEC data --------------
#
# These checks protect the public boundary of the project. They parse every R
# source file, verify the exact released-workbook allowlist and schema, reject
# identifier-like columns, and confirm that sex-specific counts reconcile with
# the both-sex totals.

project_root <- normalizePath(
  file.path(dirname(commandArgs(trailingOnly = FALSE)[
    grepl("^--file=", commandArgs(trailingOnly = FALSE))
  ] |> sub("^--file=", "", x = _)), ".."),
  winslash = "/",
  mustWork = TRUE
)

r_files <- list.files(
  project_root,
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)
r_files <- r_files[!grepl("/renv/library/", gsub("\\\\", "/", r_files))]
for (path in r_files) {
  parse(file = path, keep.source = FALSE)
}

intermediate_directory <- file.path(project_root, "data", "intermediate")
expected_files <- c(
  "Base_finalA_N_aleatoria_cohorte_actual_semilla_20260923.xlsx",
  "Base_finalA_N_aleatoria_hombres_cohorte_actual_semilla_20260923.xlsx",
  "Base_finalA_N_aleatoria_mujeres_cohorte_actual_semilla_20260923.xlsx",
  "Base_finalB_N_aleatoria_cohorte_actual_semilla_20260923.xlsx",
  "Base_finalB_N_aleatoria_hombres_cohorte_actual_semilla_20260923.xlsx",
  "Base_finalB_N_aleatoria_mujeres_cohorte_actual_semilla_20260923.xlsx"
)
actual_files <- sort(list.files(intermediate_directory, pattern = "\\.xlsx$"))
stopifnot(identical(actual_files, sort(expected_files)))

expected_columns <- c(
  "Edad_2", "pop", "fit_observed_n", "fit_positive_n", "colonoscopy_n",
  "adenoma_total", "ad_small", "ad_medium", "ad_large", "low_risk",
  "high_risk", "cancer", "ads1", "adm1", "adl1", "ads2", "adm2",
  "adl2", "ads3m", "adm3m", "adl3m",
  "adenoma_sin_clasificacion_tamano", "adenoma_sin_clasificacion_riesgo"
)
prohibited_column_pattern <- paste(
  c("rut", "codigo", "fecha", "nacimiento", "nombre", "email", "centro"),
  collapse = "|"
)

read_intermediate <- function(file_name) {
  table <- readxl::read_excel(file.path(intermediate_directory, file_name))
  stopifnot(identical(names(table), expected_columns))
  stopifnot(!any(grepl(prohibited_column_pattern, names(table), ignore.case = TRUE)))
  stopifnot(all(vapply(table, is.numeric, logical(1))))
  count_columns <- setdiff(names(table), "Edad_2")
  stopifnot(sum(is.na(table$Edad_2)) <= 1L)
  table[count_columns][is.na(table[count_columns])] <- 0
  stopifnot(all(as.matrix(table[count_columns]) >= 0))
  stopifnot(all(as.matrix(table[count_columns]) == round(as.matrix(table[count_columns]))))
  stopifnot(all(as.matrix(table[setdiff(count_columns, "pop")]) <= table$pop))
  as.data.frame(table)
}

tables <- setNames(lapply(expected_files, read_intermediate), expected_files)

check_sex_reconciliation <- function(prefix) {
  total_name <- paste0(
    "Base_final", prefix,
    "_N_aleatoria_cohorte_actual_semilla_20260923.xlsx"
  )
  men_name <- paste0(
    "Base_final", prefix,
    "_N_aleatoria_hombres_cohorte_actual_semilla_20260923.xlsx"
  )
  women_name <- paste0(
    "Base_final", prefix,
    "_N_aleatoria_mujeres_cohorte_actual_semilla_20260923.xlsx"
  )

  total <- tables[[total_name]]
  men <- tables[[men_name]]
  women <- tables[[women_name]]
  ages <- sort(
    unique(c(total$Edad_2, men$Edad_2, women$Edad_2)),
    na.last = TRUE
  )
  count_columns <- setdiff(expected_columns, "Edad_2")

  align <- function(table) {
    output <- merge(data.frame(Edad_2 = ages), table, by = "Edad_2", all.x = TRUE)
    output[count_columns][is.na(output[count_columns])] <- 0
    output
  }

  total <- align(total)
  men <- align(men)
  women <- align(women)
  stopifnot(all(as.matrix(total[count_columns]) ==
    as.matrix(men[count_columns]) + as.matrix(women[count_columns])))
}

check_sex_reconciliation("A")
check_sex_reconciliation("B")

message("Repository checks passed for ", length(r_files), " R files and ",
        length(expected_files), " released workbooks.")

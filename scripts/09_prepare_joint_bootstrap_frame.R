# -*- coding: UTF-8 -*-
# Prepare the restricted participant-level frame for the joint bootstrap ------
#
# This stage is intentionally separate from the open-data pipeline. It rebuilds
# the current PRENEC cohort from the authorised regional workbooks, retains no
# direct identifiers, and writes one row per analytical participant to an RDS
# under data/private (or PRENEC_BOOTSTRAP_FRAME). The RDS remains excluded from
# version control. Counts reconstructed from the frame must exactly match the
# six released A/B age-aggregated workbooks before the file is saved.

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

options(stringsAsFactors = FALSE)

all_arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", all_arguments, value = TRUE)
if (length(file_argument) != 1L) {
  stop("This script must be run with Rscript.", call. = FALSE)
}

script_path <- normalizePath(
  sub("^--file=", "", file_argument[[1]]),
  winslash = "/",
  mustWork = TRUE
)
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  winslash = "/",
  mustWork = TRUE
)

frame_path <- Sys.getenv(
  "PRENEC_BOOTSTRAP_FRAME",
  unset = file.path(
    project_root,
    "data",
    "private",
    "prenec_joint_bootstrap_frame.rds"
  )
)
released_base_directory <- Sys.getenv(
  "PRENEC_RELEASED_BASE_DIR",
  unset = file.path(project_root, "data", "intermediate")
)
build_work_directory <- Sys.getenv(
  "PRENEC_BOOTSTRAP_BUILD_DIR",
  unset = file.path(project_root, ".work", "joint_bootstrap_frame")
)
manifest_path <- Sys.getenv(
  "PRENEC_BOOTSTRAP_FRAME_MANIFEST",
  unset = file.path(
    project_root,
    "results",
    "joint_bootstrap",
    "frame_manifest.tsv"
  )
)

dir.create(dirname(frame_path), recursive = TRUE, showWarnings = FALSE)
dir.create(build_work_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)

# The implementation creates the selected A/B FIT assignments and all outcome
# definitions. Its ordinary aggregate outputs are redirected to ignored working
# directories; the released workbooks are used only for the exact reconciliation
# below.
Sys.setenv(
  PRENEC_PROJECT_ROOT = project_root,
  PRENEC_USAR_BASE_HISTORICA = "0",
  PRENEC_OUTPUT_DIR = file.path(build_work_directory, "intermediate"),
  PRENEC_AUDIT_DIR = file.path(build_work_directory, "quality_control")
)

build_script <- file.path(
  project_root,
  "R",
  "pipeline",
  "build_ab_databases.R"
)
if (!file.exists(build_script)) {
  stop("The A/B database constructor was not found: ", build_script, call. = FALSE)
}

# Evaluating the parsed expression avoids a Windows `source()` connection issue
# observed when the process starts without an available UTF-8 LC_CTYPE locale.
eval(
  parse(file = build_script, encoding = "UTF-8", keep.source = FALSE),
  envir = .GlobalEnv
)

if (!exists("prenec", inherits = FALSE)) {
  stop("The A/B constructor did not return the participant-level PRENEC object.", call. = FALSE)
}

age_group <- function(age) {
  cut(
    age,
    breaks = c(49, 54, 59, 64, 69, 74, 75),
    labels = c("50-54", "55-59", "60-64", "65-69", "70-74", "75"),
    right = TRUE,
    include.lowest = TRUE,
    ordered_result = TRUE
  )
}

cohort <- prenec %>%
  filter(
    highrisk == 0,
    tiene_fit_observado,
    !is.na(Edad_2),
    Edad_2 >= 50,
    Edad_2 <= 75,
    Sexo_analisis %in% c("Hombre", "Mujer")
  )

bootstrap_frame <- cohort %>%
  transmute(
    Age = as.integer(Edad_2),
    Age_group = as.character(age_group(Edad_2)),
    Sex = if_else(Sexo_analisis == "Hombre", "Men", "Women"),
    FIT_A_positive = as.integer(FIT_A == "Positivo"),
    FIT_B_positive = as.integer(FIT_B == "Positivo"),
    Colonoscopy_qualifying = as.integer(!is.na(Indicador_Fecha_Colono)),
    Low_risk = as.integer(!is.na(risk) & risk == "low_risk"),
    High_risk = as.integer(!is.na(risk) & risk == "high_risk"),
    Cancer = as.integer(!is.na(risk) & risk == "cancer"),
    Any_adenoma = as.integer(!is.na(adenoma) & adenoma == 1)
  )

binary_columns <- c(
  "FIT_A_positive", "FIT_B_positive", "Colonoscopy_qualifying",
  "Low_risk", "High_risk", "Cancer", "Any_adenoma"
)

if (nrow(bootstrap_frame) == 0L ||
    anyNA(bootstrap_frame[, c("Age", "Age_group", "Sex", binary_columns)]) ||
    any(!as.matrix(bootstrap_frame[, binary_columns]) %in% c(0L, 1L)) ||
    anyDuplicated(names(bootstrap_frame))) {
  stop("The participant bootstrap frame failed structural validation.", call. = FALSE)
}

if (any(bootstrap_frame$Low_risk + bootstrap_frame$High_risk >
        bootstrap_frame$Any_adenoma)) {
  stop("Risk-classified adenomas exceed the direct any-adenoma indicator.", call. = FALSE)
}

lesion_columns <- c(
  adenoma_total = "Any_adenoma",
  low_risk = "Low_risk",
  high_risk = "High_risk",
  cancer = "Cancer"
)

counts_from_frame <- function(data, base_id) {
  fit_column <- paste0("FIT_", base_id, "_positive")
  data %>%
    group_by(Age) %>%
    summarise(
      pop = n(),
      fit_observed_n = n(),
      fit_positive_n = sum(.data[[fit_column]]),
      colonoscopy_n = sum(
        .data[[fit_column]] * Colonoscopy_qualifying
      ),
      adenoma_total = sum(
        .data[[fit_column]] * Colonoscopy_qualifying * Any_adenoma
      ),
      low_risk = sum(
        .data[[fit_column]] * Colonoscopy_qualifying * Low_risk
      ),
      high_risk = sum(
        .data[[fit_column]] * Colonoscopy_qualifying * High_risk
      ),
      cancer = sum(
        .data[[fit_column]] * Colonoscopy_qualifying * Cancer
      ),
      .groups = "drop"
    ) %>%
    rename(Edad_2 = Age) %>%
    arrange(Edad_2)
}

analysis_grid <- data.frame(
  Analysis = c(
    "A_Total", "A_Men", "A_Women",
    "B_Total", "B_Men", "B_Women"
  ),
  Base = rep(c("A", "B"), each = 3L),
  Sex = rep(c("Both sexes", "Men", "Women"), times = 2L),
  File_suffix = rep(c("", "_hombres", "_mujeres"), times = 2L),
  stringsAsFactors = FALSE
)

comparison_columns <- c(
  "pop", "fit_observed_n", "fit_positive_n", "colonoscopy_n",
  names(lesion_columns)
)
reconciliation_rows <- vector("list", nrow(analysis_grid))

for (i in seq_len(nrow(analysis_grid))) {
  specification <- analysis_grid[i, , drop = FALSE]
  selected <- bootstrap_frame
  if (specification$Sex != "Both sexes") {
    selected <- selected[selected$Sex == specification$Sex, , drop = FALSE]
  }
  reconstructed <- counts_from_frame(selected, specification$Base)

  released_file <- file.path(
    released_base_directory,
    paste0(
      "Base_final", specification$Base,
      "_N_aleatoria", specification$File_suffix,
      "_cohorte_actual_semilla_", SEMILLA_SORTEO, ".xlsx"
    )
  )
  if (!file.exists(released_file)) {
    stop("Released comparison workbook not found: ", released_file, call. = FALSE)
  }
  released <- as.data.frame(readxl::read_excel(released_file))
  released <- released[
    !is.na(released$Edad_2) & released$Edad_2 >= 50 & released$Edad_2 <= 75,
    c("Edad_2", comparison_columns),
    drop = FALSE
  ]
  released[comparison_columns][is.na(released[comparison_columns])] <- 0
  released <- released[order(released$Edad_2), , drop = FALSE]

  check <- merge(
    reconstructed,
    released,
    by = "Edad_2",
    suffixes = c("_frame", "_released"),
    all = TRUE
  )
  differences <- vapply(comparison_columns, function(column) {
    max(abs(
      check[[paste0(column, "_frame")]] -
        check[[paste0(column, "_released")]]
    ), na.rm = TRUE)
  }, numeric(1))
  if (any(!is.finite(differences)) || any(differences != 0)) {
    stop(
      "Participant frame does not reconcile with released counts for ",
      specification$Analysis,
      ".",
      call. = FALSE
    )
  }

  reconciliation_rows[[i]] <- data.frame(
    Analysis = specification$Analysis,
    Base = specification$Base,
    Sex = specification$Sex,
    Population = sum(reconstructed$pop),
    FIT_positive = sum(reconstructed$fit_positive_n),
    Colonoscopy_qualifying = sum(reconstructed$colonoscopy_n),
    Maximum_count_difference = max(differences),
    stringsAsFactors = FALSE
  )
}

reconciliation <- do.call(rbind, reconciliation_rows)
if (any(reconciliation$Population <= 0L) ||
    any(reconciliation$FIT_positive <= 0L) ||
    any(reconciliation$Colonoscopy_qualifying <= 0L)) {
  stop("One or more reconstructed analyses has invalid nested counts.", call. = FALSE)
}

stratum_manifest <- bootstrap_frame %>%
  count(Sex, Age_group, name = "Population") %>%
  arrange(Sex, Age_group)

saveRDS(bootstrap_frame, frame_path, compress = "xz", version = 3)

manifest <- rbind(
  data.frame(
    Record_type = "analysis",
    Group_1 = reconciliation$Analysis,
    Group_2 = reconciliation$Sex,
    Population = reconciliation$Population,
    FIT_positive = reconciliation$FIT_positive,
    Colonoscopy_qualifying = reconciliation$Colonoscopy_qualifying,
    Maximum_count_difference = reconciliation$Maximum_count_difference,
    stringsAsFactors = FALSE
  ),
  data.frame(
    Record_type = "bootstrap_stratum",
    Group_1 = stratum_manifest$Sex,
    Group_2 = stratum_manifest$Age_group,
    Population = stratum_manifest$Population,
    FIT_positive = NA_real_,
    Colonoscopy_qualifying = NA_real_,
    Maximum_count_difference = NA_real_,
    stringsAsFactors = FALSE
  )
)

write.table(
  manifest,
  manifest_path,
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

message(
  "Joint-bootstrap frame prepared: ", nrow(bootstrap_frame),
  " participants in ", nrow(stratum_manifest), " age-sex strata."
)
message("Restricted frame: ", normalizePath(frame_path, winslash = "/"))
message("Non-identifying manifest: ", normalizePath(manifest_path, winslash = "/"))

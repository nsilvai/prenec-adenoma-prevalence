# -*- coding: UTF-8 -*-

# Stage 10: joint participant-bootstrap uncertainty analysis -----------------
#
# This is the primary uncertainty analysis used by the current manuscript.
# It remains separate from Stage 06 and does not read from or overwrite the
# Stage 06 aggregate-analysis output directory.
#
# For assignment a, lesion j, and sex-by-age stratum h, define
#
#   N[h]       analytical participants
#   F[a,h]     participants with a positive selected FIT
#   C[a,h]     F-positive participants with a qualifying colonoscopy
#   D[a,j,h]   participants in C with lesion j detected histologically
#
# The empirical pathway component is
#
#   Q[a,j] = sum_h (N[h] / N) (F[a,h] / N[h]) (D[a,j,h] / C[a,h])
#          = sum_h F[a,h] D[a,j,h] / {N C[a,h]}.
#
# This is algebraically identical to correcting D/N by C/F within strata, but
# it keeps the nested counts together. A stratified nonparametric bootstrap is
# implemented by drawing multinomial counts over the observed participant
# patterns within each Sex x Age_group stratum. The same bootstrap bank is used
# for assignments A and B, every lesion, all reported populations, and both FIT
# sensitivity scenarios.
#
# Sensitivity uncertainty is drawn independently of the participant bootstrap:
#
#   r[a,j] = Q[a,j] / SeFIT[j]
#   p[a,j] = Q[a,j] / {SeFIT[j] SeCOL[j]}.
#
# Four uncertainty runs are exported: empirical data only, FIT sensitivity
# only, colonoscopy sensitivity only, and the full analysis. Point estimates
# are always plug-in estimates from the observed counts and point sensitivities;
# simulation draws are used only for uncertainty summaries.

suppressPackageStartupMessages({
  library(readxl)
})

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------

script_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  dirname(normalizePath(
    sub("^--file=", "", file_arg[[1]]),
    winslash = "/",
    mustWork = TRUE
  ))
}

project_directory <- Sys.getenv(
  "PRENEC_PROJECT_ROOT",
  unset = normalizePath(
    file.path(script_directory(), ".."),
    winslash = "/",
    mustWork = TRUE
  )
)

bootstrap_frame_file <- Sys.getenv("PRENEC_BOOTSTRAP_FRAME", unset = "")
if (!nzchar(bootstrap_frame_file)) {
  stop(
    "PRENEC_BOOTSTRAP_FRAME must point to the restricted deidentified participant frame.",
    call. = FALSE
  )
}

sensitivity_cohort_file <- Sys.getenv(
  "PRENEC_BASE_SENS",
  unset = file.path(project_directory, "data", "private", "Base_prenec.xlsx")
)
colonoscopy_parameter_file <- Sys.getenv(
  "PRENEC_COL_PARAMS",
  unset = file.path(
    project_directory,
    "data",
    "parameters",
    "colonoscopy_sensitivity_parameters.xlsx"
  )
)
external_fit_parameter_file <- Sys.getenv(
  "PRENEC_FIT_PARAMS_PUBLICATION",
  unset = file.path(
    project_directory,
    "data",
    "parameters",
    "external_fit_sensitivity_parameters_combined.xlsx"
  )
)
output_directory <- Sys.getenv(
  "PRENEC_JOINT_BOOTSTRAP_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "joint_bootstrap")
)

general_seed <- Sys.getenv("PRENEC_SEED", unset = "20260923")
seed_sensitivity_selection <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_SENS",
  unset = general_seed
)))
seed_bootstrap <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_BOOTSTRAP",
  unset = "20261008"
)))
seed_local_fit <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_FIT_LOCAL_BOOTSTRAP",
  unset = "20261009"
)))
seed_literature_fit <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_FIT_LITERATURE_BOOTSTRAP",
  unset = "20261010"
)))
seed_colonoscopy <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_COLONOSCOPY_BOOTSTRAP",
  unset = "20261011"
)))
n_bootstrap <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_N_BOOTSTRAP",
  unset = Sys.getenv("PRENEC_N_SIM", unset = "10000")
)))

numeric_configuration <- c(
  seed_sensitivity_selection,
  seed_bootstrap,
  seed_local_fit,
  seed_literature_fit,
  seed_colonoscopy,
  n_bootstrap
)
if (anyNA(numeric_configuration) || n_bootstrap < 2L) {
  stop("Bootstrap seeds and PRENEC_N_BOOTSTRAP must be valid integers.", call. = FALSE)
}

required_files <- c(
  bootstrap_frame_file,
  sensitivity_cohort_file,
  colonoscopy_parameter_file,
  external_fit_parameter_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Required input files were not found:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

lesion_keys <- c("low_risk", "high_risk", "cancer", "adenoma_total")
lesion_labels <- c(
  low_risk = "Low-risk adenoma",
  high_risk = "High-risk adenoma",
  cancer = "Colorectal cancer",
  adenoma_total = "Any adenoma"
)
lesion_frame_columns <- c(
  low_risk = "Low_risk",
  high_risk = "High_risk",
  cancer = "Cancer",
  adenoma_total = "Any_adenoma"
)
age_groups <- c("50-54", "55-59", "60-64", "65-69", "70-74", "75")
sex_levels <- c("Men", "Women")
assignments <- c("A", "B")
populations <- c("Both sexes", "Men", "Women")
fit_scenarios <- c("PRENEC", "Literature")
estimands <- c(
  "Lesion detection under universal colonoscopy",
  "Prevalence"
)
uncertainty_runs <- c(
  "Empirical data only",
  "FIT sensitivity only",
  "Colonoscopy sensitivity only",
  "Full analysis"
)

age_group_from_age <- function(age) {
  ifelse(
    age >= 50 & age <= 54, "50-54",
    ifelse(
      age >= 55 & age <= 59, "55-59",
      ifelse(
        age >= 60 & age <= 64, "60-64",
        ifelse(
          age >= 65 & age <= 69, "65-69",
          ifelse(
            age >= 70 & age <= 74, "70-74",
            ifelse(age == 75, "75", NA_character_)
          )
        )
      )
    )
  )
}

normalise_logical <- function(x, field) {
  if (is.logical(x)) {
    if (anyNA(x)) {
      stop(field, " contains missing values.", call. = FALSE)
    }
    return(x)
  }
  if (is.numeric(x) || is.integer(x)) {
    if (anyNA(x) || any(!x %in% c(0, 1))) {
      stop(field, " must contain only 0 and 1.", call. = FALSE)
    }
    return(as.logical(x))
  }
  text <- tolower(trimws(as.character(x)))
  result <- rep(NA, length(text))
  result[text %in% c("true", "t", "1", "yes", "si", "sí")] <- TRUE
  result[text %in% c("false", "f", "0", "no")] <- FALSE
  if (anyNA(result)) {
    stop(field, " contains values that cannot be interpreted as binary.", call. = FALSE)
  }
  result
}

open_uniform_matrix <- function(nrow, ncol) {
  value <- matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
  pmin(pmax(value, .Machine$double.eps), 1 - .Machine$double.eps)
}

write_tsv <- function(x, filename) {
  write.table(
    x,
    file = file.path(output_directory, filename),
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "NA",
    fileEncoding = "UTF-8"
  )
}

# -----------------------------------------------------------------------------
# 2. Restricted deidentified participant frame
# -----------------------------------------------------------------------------

read_participant_frame <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension == "rds") {
    object <- readRDS(path)
  } else if (extension == "csv") {
    object <- read.csv(path, check.names = FALSE)
  } else if (extension %in% c("tsv", "txt")) {
    object <- read.delim(path, check.names = FALSE)
  } else {
    stop(
      "PRENEC_BOOTSTRAP_FRAME must be an .rds, .csv, .tsv, or tab-delimited .txt file.",
      call. = FALSE
    )
  }
  if (!is.data.frame(object)) {
    stop("PRENEC_BOOTSTRAP_FRAME must contain one data.frame.", call. = FALSE)
  }
  as.data.frame(object, stringsAsFactors = FALSE)
}

participant_frame <- read_participant_frame(bootstrap_frame_file)
required_frame_columns <- c(
  "Age", "Age_group", "Sex",
  "FIT_A_positive", "FIT_B_positive", "Colonoscopy_qualifying",
  unname(lesion_frame_columns)
)
missing_frame_columns <- setdiff(required_frame_columns, names(participant_frame))
if (length(missing_frame_columns) > 0L) {
  stop(
    "The participant bootstrap frame is missing: ",
    paste(missing_frame_columns, collapse = ", "),
    call. = FALSE
  )
}

participant_frame <- participant_frame[, required_frame_columns, drop = FALSE]
participant_frame$Age <- suppressWarnings(as.integer(participant_frame$Age))
participant_frame$Age_group <- trimws(as.character(participant_frame$Age_group))
participant_frame$Sex <- trimws(as.character(participant_frame$Sex))

binary_frame_columns <- c(
  "FIT_A_positive", "FIT_B_positive", "Colonoscopy_qualifying",
  unname(lesion_frame_columns)
)
for (field in binary_frame_columns) {
  participant_frame[[field]] <- as.integer(normalise_logical(
    participant_frame[[field]],
    field
  ))
}

derived_age_group <- age_group_from_age(participant_frame$Age)
if (nrow(participant_frame) == 0L ||
    anyNA(participant_frame$Age) ||
    anyNA(derived_age_group) ||
    any(!participant_frame$Sex %in% sex_levels) ||
    any(!participant_frame$Age_group %in% age_groups) ||
    any(participant_frame$Age_group != derived_age_group)) {
  stop(
    "The participant frame must contain complete ages 50-75, matching age groups, and Sex coded Men/Women.",
    call. = FALSE
  )
}

if (nrow(participant_frame) != 31104L ||
    sum(participant_frame$Sex == "Men") != 8267L ||
    sum(participant_frame$Sex == "Women") != 22837L) {
  stop(
    "The participant frame does not reproduce the prespecified analytical population (N=31,104; Men=8,267; Women=22,837).",
    call. = FALSE
  )
}

if (any(participant_frame$Low_risk + participant_frame$High_risk >
        participant_frame$Any_adenoma)) {
  stop(
    "Low-risk plus high-risk adenoma exceeds the direct any-adenoma indicator.",
    call. = FALSE
  )
}

participant_frame$Stratum <- paste(
  participant_frame$Sex,
  participant_frame$Age_group,
  sep = "::"
)
stratum_grid <- expand.grid(
  Sex = sex_levels,
  Age_group = age_groups,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
stratum_grid$Stratum <- paste(
  stratum_grid$Sex,
  stratum_grid$Age_group,
  sep = "::"
)
stratum_sizes <- table(factor(
  participant_frame$Stratum,
  levels = stratum_grid$Stratum
))
if (any(stratum_sizes <= 0L)) {
  stop("Every Sex x Age_group bootstrap stratum must be non-empty.", call. = FALSE)
}

population_sizes <- c(
  "Both sexes" = nrow(participant_frame),
  "Men" = sum(participant_frame$Sex == "Men"),
  "Women" = sum(participant_frame$Sex == "Women")
)

# -----------------------------------------------------------------------------
# 3. One joint stratified participant-bootstrap bank
# -----------------------------------------------------------------------------

q_bootstrap <- array(
  0,
  dim = c(n_bootstrap, length(assignments), length(populations), length(lesion_keys)),
  dimnames = list(
    Replicate = NULL,
    Assignment = assignments,
    Population = populations,
    Lesion = lesion_keys
  )
)
q_plugin <- array(
  0,
  dim = c(length(assignments), length(populations), length(lesion_keys)),
  dimnames = list(
    Assignment = assignments,
    Population = populations,
    Lesion = lesion_keys
  )
)

pattern_columns <- c(
  "FIT_A_positive", "FIT_B_positive", "Colonoscopy_qualifying",
  unname(lesion_frame_columns)
)
observed_count_rows <- list()
observed_row_index <- 0L

RNGkind(
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)
set.seed(seed_bootstrap)

for (stratum_index in seq_len(nrow(stratum_grid))) {
  stratum_context <- stratum_grid[stratum_index, , drop = FALSE]
  stratum_data <- participant_frame[
    participant_frame$Stratum == stratum_context$Stratum,
    pattern_columns,
    drop = FALSE
  ]
  stratum_n <- nrow(stratum_data)

  patterns <- aggregate(
    rep.int(1L, stratum_n),
    by = stratum_data,
    FUN = sum
  )
  names(patterns)[ncol(patterns)] <- "Observed_n"
  pattern_order <- do.call(order, patterns[pattern_columns])
  patterns <- patterns[pattern_order, , drop = FALSE]
  rownames(patterns) <- NULL

  bootstrap_pattern_counts <- rmultinom(
    n = n_bootstrap,
    size = stratum_n,
    prob = patterns$Observed_n / stratum_n
  )
  if (any(colSums(bootstrap_pattern_counts) != stratum_n)) {
    stop("A multinomial bootstrap sample did not preserve its stratum size.", call. = FALSE)
  }

  affected_populations <- c("Both sexes", stratum_context$Sex)

  for (assignment in assignments) {
    fit_field <- paste0("FIT_", assignment, "_positive")
    pattern_f <- as.integer(patterns[[fit_field]] == 1L)
    pattern_c <- as.integer(
      patterns[[fit_field]] == 1L & patterns$Colonoscopy_qualifying == 1L
    )
    observed_f <- sum(patterns$Observed_n * pattern_f)
    observed_c <- sum(patterns$Observed_n * pattern_c)
    bootstrap_f <- as.numeric(crossprod(pattern_f, bootstrap_pattern_counts))
    bootstrap_c <- as.numeric(crossprod(pattern_c, bootstrap_pattern_counts))

    if (observed_f <= 0L || observed_c <= 0L || observed_c > observed_f ||
        any(bootstrap_f < bootstrap_c) || any(bootstrap_c <= 0L)) {
      stop(
        "A bootstrap stratum has undefined adherence (F+ or C equals zero) or violates C <= F+: ",
        stratum_context$Stratum,
        ", assignment ", assignment, ".",
        call. = FALSE
      )
    }

    for (lesion in lesion_keys) {
      lesion_field <- unname(lesion_frame_columns[[lesion]])
      pattern_d <- as.integer(pattern_c == 1L & patterns[[lesion_field]] == 1L)
      observed_d <- sum(patterns$Observed_n * pattern_d)
      bootstrap_d <- as.numeric(crossprod(pattern_d, bootstrap_pattern_counts))

      if (observed_d > observed_c || any(bootstrap_d > bootstrap_c)) {
        stop(
          "Detected lesions are not nested within qualifying colonoscopies: ",
          stratum_context$Stratum, ", assignment ", assignment,
          ", lesion ", lesion, ".",
          call. = FALSE
        )
      }

      observed_lhs <- (observed_d / stratum_n) / (observed_c / observed_f)
      observed_rhs <- (observed_f / stratum_n) * (observed_d / observed_c)
      if (!isTRUE(all.equal(observed_lhs, observed_rhs, tolerance = 1e-14))) {
        stop("The two algebraic forms are not equal for an observed stratum.", call. = FALSE)
      }

      observed_row_index <- observed_row_index + 1L
      observed_count_rows[[observed_row_index]] <- data.frame(
        Sex = stratum_context$Sex,
        Age_group = stratum_context$Age_group,
        Assignment = assignment,
        Lesion_key = lesion,
        Lesion = unname(lesion_labels[[lesion]]),
        N = stratum_n,
        FIT_positive = observed_f,
        Colonoscopy_qualifying = observed_c,
        Detected = observed_d,
        FIT_positivity = observed_f / stratum_n,
        Colonoscopy_yield = observed_d / observed_c,
        Detection_over_adherence = observed_lhs,
        Positivity_times_yield = observed_rhs,
        Absolute_identity_difference = abs(observed_lhs - observed_rhs),
        stringsAsFactors = FALSE
      )

      for (population in affected_populations) {
        denominator <- unname(population_sizes[[population]])
        q_plugin[assignment, population, lesion] <-
          q_plugin[assignment, population, lesion] +
          observed_f * observed_d / (denominator * observed_c)
        q_bootstrap[, assignment, population, lesion] <-
          q_bootstrap[, assignment, population, lesion] +
          bootstrap_f * bootstrap_d / (denominator * bootstrap_c)
      }
    }
  }
}

observed_counts <- do.call(rbind, observed_count_rows)
rownames(observed_counts) <- NULL

if (any(observed_counts$Colonoscopy_qualifying > observed_counts$FIT_positive) ||
    any(observed_counts$Detected > observed_counts$Colonoscopy_qualifying) ||
    max(observed_counts$Absolute_identity_difference) > 1e-12 ||
    any(!is.finite(q_plugin)) ||
    any(!is.finite(q_bootstrap)) ||
    any(q_plugin <= 0) ||
    any(q_bootstrap <= 0)) {
  stop("Joint-count or empirical-component validation failed.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 4. PRENEC-derived FIT sensitivity
# -----------------------------------------------------------------------------

sensitivity_data <- as.data.frame(readxl::read_excel(sensitivity_cohort_file))
required_sensitivity_columns <- c(
  "Codigo", "highrisk", "Tipo_Histologico_1", "Tamano_mm",
  "Valor_TSDO_1_1", "Valor_TSDO_2_1"
)
missing_sensitivity_columns <- setdiff(
  required_sensitivity_columns,
  names(sensitivity_data)
)
if (length(missing_sensitivity_columns) > 0L) {
  stop(
    "The PRENEC sensitivity cohort is missing: ",
    paste(missing_sensitivity_columns, collapse = ", "),
    call. = FALSE
  )
}

sensitivity_data$highrisk <- suppressWarnings(as.numeric(sensitivity_data$highrisk))
sensitivity_data <- sensitivity_data[
  which(sensitivity_data$highrisk == 1),
  required_sensitivity_columns,
  drop = FALSE
]
if (nrow(sensitivity_data) == 0L ||
    anyNA(sensitivity_data$Codigo) ||
    anyDuplicated(sensitivity_data$Codigo)) {
  stop(
    "The PRENEC sensitivity cohort requires one row per high-risk participant.",
    call. = FALSE
  )
}

adenoma_histologies <- c(
  "Adenoma tubular con atipia de bajo grado",
  "Adenoma tubular con atipia de alto grado",
  "Adenoma tubulo-velloso con atipia de bajo grado",
  "Adenoma tubulo-velloso con atipia de alto grado",
  "Adenoma velloso con atipia de alto grado",
  "Adenocarcinoma intramucoso"
)
invasive_cancer_histologies <- c(
  "Adenocarcinoma bien diferenciado",
  "Adenocarcinoma moderadamente diferenciado",
  "Adenocarcinoma pobremente diferenciado"
)

sensitivity_data$Tamano_mm <- suppressWarnings(as.numeric(sensitivity_data$Tamano_mm))
sensitivity_data$adenoma_present <- as.integer(
  sensitivity_data$Tipo_Histologico_1 %in% adenoma_histologies
)
sensitivity_data$adenoma_risk <- ifelse(
  sensitivity_data$Tipo_Histologico_1 == adenoma_histologies[[1]] &
    !is.na(sensitivity_data$Tamano_mm) & sensitivity_data$Tamano_mm >= 10,
  1L,
  ifelse(
    sensitivity_data$Tipo_Histologico_1 %in% adenoma_histologies[-1],
    1L,
    ifelse(
      sensitivity_data$Tipo_Histologico_1 == adenoma_histologies[[1]] &
        !is.na(sensitivity_data$Tamano_mm) & sensitivity_data$Tamano_mm < 10,
      0L,
      NA_integer_
    )
  )
)
sensitivity_data$cancer_present <- as.integer(
  sensitivity_data$Tipo_Histologico_1 %in% invasive_cancer_histologies
)
sensitivity_data$FIT_1 <- ifelse(
  is.na(sensitivity_data$Valor_TSDO_1_1),
  NA,
  suppressWarnings(as.numeric(sensitivity_data$Valor_TSDO_1_1)) >= 100
)
sensitivity_data$FIT_2 <- ifelse(
  is.na(sensitivity_data$Valor_TSDO_2_1),
  NA,
  suppressWarnings(as.numeric(sensitivity_data$Valor_TSDO_2_1)) >= 100
)

participant_order <- order(
  as.character(sensitivity_data$Codigo),
  method = "radix"
)
set.seed(seed_sensitivity_selection)
selection_uniform <- numeric(nrow(sensitivity_data))
selection_uniform[participant_order] <- runif(nrow(sensitivity_data))
selected_test <- ifelse(
  !is.na(sensitivity_data$FIT_1) & !is.na(sensitivity_data$FIT_2),
  ifelse(selection_uniform < 0.5, 1L, 2L),
  ifelse(
    !is.na(sensitivity_data$FIT_1),
    1L,
    ifelse(!is.na(sensitivity_data$FIT_2), 2L, NA_integer_)
  )
)
sensitivity_data$selected_FIT <- ifelse(
  selected_test == 1L,
  sensitivity_data$FIT_1,
  ifelse(selected_test == 2L, sensitivity_data$FIT_2, NA)
)

local_lesion_status <- list(
  low_risk = as.integer(
    sensitivity_data$adenoma_present == 1L & sensitivity_data$adenoma_risk == 0L
  ),
  high_risk = as.integer(
    sensitivity_data$adenoma_present == 1L & sensitivity_data$adenoma_risk == 1L
  ),
  cancer = sensitivity_data$cancer_present,
  adenoma_total = sensitivity_data$adenoma_present
)

local_fit_rows <- lapply(lesion_keys, function(lesion) {
  cases <- !is.na(local_lesion_status[[lesion]]) &
    local_lesion_status[[lesion]] == 1L &
    !is.na(sensitivity_data$selected_FIT)
  total <- sum(cases)
  true_positive <- sum(cases & sensitivity_data$selected_FIT)
  data.frame(
    Lesion_key = lesion,
    Lesion = unname(lesion_labels[[lesion]]),
    True_positive = true_positive,
    False_negative = total - true_positive,
    Estimate = true_positive / total,
    Beta_a = true_positive + 0.5,
    Beta_b = total - true_positive + 0.5,
    Distribution = "Jeffreys beta",
    stringsAsFactors = FALSE
  )
})
local_fit_parameters <- do.call(rbind, local_fit_rows)
rownames(local_fit_parameters) <- NULL

if (any(!is.finite(local_fit_parameters$Estimate)) ||
    any(local_fit_parameters$Estimate <= 0 | local_fit_parameters$Estimate > 1)) {
  stop("PRENEC-derived FIT sensitivity parameters are invalid.", call. = FALSE)
}

cancer_local_row <- local_fit_parameters[
  local_fit_parameters$Lesion_key == "cancer",
  ,
  drop = FALSE
]
if (nrow(cancer_local_row) != 1L ||
    cancer_local_row$True_positive != 6L ||
    cancer_local_row$False_negative != 0L ||
    cancer_local_row$Beta_a != 6.5 ||
    cancer_local_row$Beta_b != 0.5) {
  stop(
    "The prespecified PRENEC cancer FIT distribution requires TP=6 and FN=0, giving Beta(6.5, 0.5).",
    call. = FALSE
  )
}

set.seed(seed_local_fit)
local_fit_uniforms <- open_uniform_matrix(n_bootstrap, length(lesion_keys))
local_fit_draws <- vapply(seq_along(lesion_keys), function(j) {
  row <- local_fit_parameters[
    local_fit_parameters$Lesion_key == lesion_keys[[j]],
    ,
    drop = FALSE
  ]
  qbeta(local_fit_uniforms[, j], row$Beta_a[[1]], row$Beta_b[[1]])
}, numeric(n_bootstrap))
colnames(local_fit_draws) <- lesion_keys

# -----------------------------------------------------------------------------
# 5. Literature FIT sensitivity and colonoscopy sensitivity
# -----------------------------------------------------------------------------

read_external_fit_parameters <- function(path) {
  table <- as.data.frame(readxl::read_excel(
    path,
    sheet = "Escenarios_literatura",
    na = c("", "NA")
  ))
  required <- c(
    "scenario_id", "lesion_key", "estimate", "ci_lower", "ci_upper",
    "beta_a", "beta_b", "distribution", "logit_mean", "logit_se",
    "prior_added_in_simulation", "active", "source_id", "citation"
  )
  missing <- setdiff(required, names(table))
  if (length(missing) > 0L) {
    stop(
      "The external FIT parameter table is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  table$active <- normalise_logical(table$active, "active")
  table$scenario_id <- trimws(as.character(table$scenario_id))
  table$lesion_key <- tolower(trimws(as.character(table$lesion_key)))
  table$distribution <- tolower(trimws(as.character(table$distribution)))
  table <- table[
    table$active &
      (
        (table$scenario_id == "meta_naa_oc100" & table$lesion_key == "low_risk") |
          (table$scenario_id == "lin_2021_oc_sensor" &
             table$lesion_key %in% c("high_risk", "cancer")) |
          (table$scenario_id == "meta_any_adenoma_oc100" &
             table$lesion_key == "adenoma_total")
      ),
    ,
    drop = FALSE
  ]
  expected <- c(
    "meta_naa_oc100::low_risk",
    "lin_2021_oc_sensor::high_risk",
    "lin_2021_oc_sensor::cancer",
    "meta_any_adenoma_oc100::adenoma_total"
  )
  observed <- paste(table$scenario_id, table$lesion_key, sep = "::")
  if (!setequal(observed, expected) || anyDuplicated(observed)) {
    stop("The four required external FIT inputs are not uniquely defined.", call. = FALSE)
  }
  numeric_fields <- c(
    "estimate", "ci_lower", "ci_upper", "beta_a", "beta_b",
    "logit_mean", "logit_se", "prior_added_in_simulation"
  )
  for (field in numeric_fields) {
    table[[field]] <- suppressWarnings(as.numeric(table[[field]]))
  }
  table <- table[match(lesion_keys, table$lesion_key), , drop = FALSE]
  if (anyNA(table$lesion_key) ||
      any(!table$distribution %in% c("beta", "logit_normal")) ||
      any(!is.finite(table$estimate)) ||
      any(table$estimate <= 0 | table$estimate >= 1)) {
    stop("The external FIT distributions are invalid.", call. = FALSE)
  }
  table
}

draw_external_fit <- function(parameters, n, seed) {
  set.seed(seed)
  uniforms <- open_uniform_matrix(n, nrow(parameters))
  draws <- vapply(seq_len(nrow(parameters)), function(j) {
    if (parameters$distribution[[j]] == "logit_normal") {
      if (!is.finite(parameters$logit_mean[[j]]) ||
          !is.finite(parameters$logit_se[[j]]) ||
          parameters$logit_se[[j]] <= 0) {
        stop("A logit-normal FIT distribution is incomplete.", call. = FALSE)
      }
      return(plogis(qnorm(
        uniforms[, j],
        mean = parameters$logit_mean[[j]],
        sd = parameters$logit_se[[j]]
      )))
    }
    a <- parameters$beta_a[[j]] + parameters$prior_added_in_simulation[[j]]
    b <- parameters$beta_b[[j]] + parameters$prior_added_in_simulation[[j]]
    if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) {
      stop("A beta FIT distribution is incomplete.", call. = FALSE)
    }
    qbeta(uniforms[, j], a, b)
  }, numeric(n))
  colnames(draws) <- parameters$lesion_key
  draws
}

external_fit_parameters <- read_external_fit_parameters(
  external_fit_parameter_file
)
literature_fit_draws <- draw_external_fit(
  external_fit_parameters,
  n_bootstrap,
  seed_literature_fit
)

read_colonoscopy_parameters <- function(path) {
  table <- as.data.frame(readxl::read_excel(
    path,
    sheet = "Parametros_modelo",
    na = c("", "NA")
  ))
  required <- c(
    "lesion_key", "metric", "estimate", "beta_a", "beta_b",
    "prior_added_in_R", "active", "used_current_model", "source_id",
    "parameter_method", "notes"
  )
  missing <- setdiff(required, names(table))
  if (length(missing) > 0L) {
    stop(
      "The colonoscopy parameter table is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  table$active <- normalise_logical(table$active, "active")
  table$used_current_model <- normalise_logical(
    table$used_current_model,
    "used_current_model"
  )
  table$lesion_key <- tolower(trimws(as.character(table$lesion_key)))
  table$metric <- tolower(trimws(as.character(table$metric)))
  numeric_fields <- c("estimate", "beta_a", "beta_b", "prior_added_in_R")
  for (field in numeric_fields) {
    table[[field]] <- suppressWarnings(as.numeric(table[[field]]))
  }
  table <- table[
    table$active & table$used_current_model &
      table$metric == "sensitivity" &
      table$lesion_key %in% lesion_keys,
    ,
    drop = FALSE
  ]
  table <- table[match(lesion_keys, table$lesion_key), , drop = FALSE]
  if (anyNA(table$lesion_key) || anyDuplicated(table$lesion_key) ||
      any(!is.finite(as.matrix(table[, numeric_fields]))) ||
      any(table$estimate <= 0 | table$estimate > 1) ||
      any(table$beta_a + table$prior_added_in_R <= 0) ||
      any(table$beta_b + table$prior_added_in_R <= 0)) {
    stop("The colonoscopy sensitivity parameters are invalid.", call. = FALSE)
  }
  table
}

colonoscopy_parameters <- read_colonoscopy_parameters(
  colonoscopy_parameter_file
)
set.seed(seed_colonoscopy)
colonoscopy_uniforms <- open_uniform_matrix(n_bootstrap, length(lesion_keys))
colonoscopy_draws <- vapply(seq_along(lesion_keys), function(j) {
  row <- colonoscopy_parameters[
    colonoscopy_parameters$lesion_key == lesion_keys[[j]],
    ,
    drop = FALSE
  ]
  qbeta(
    colonoscopy_uniforms[, j],
    row$beta_a[[1]] + row$prior_added_in_R[[1]],
    row$beta_b[[1]] + row$prior_added_in_R[[1]]
  )
}, numeric(n_bootstrap))
colnames(colonoscopy_draws) <- lesion_keys

fit_points <- rbind(
  PRENEC = setNames(local_fit_parameters$Estimate, local_fit_parameters$Lesion_key),
  Literature = setNames(external_fit_parameters$estimate, external_fit_parameters$lesion_key)
)[, lesion_keys, drop = FALSE]
fit_draws <- array(
  NA_real_,
  dim = c(n_bootstrap, length(fit_scenarios), length(lesion_keys)),
  dimnames = list(
    Replicate = NULL,
    FIT_scenario = fit_scenarios,
    Lesion = lesion_keys
  )
)
fit_draws[, "PRENEC", ] <- local_fit_draws[, lesion_keys, drop = FALSE]
fit_draws[, "Literature", ] <- literature_fit_draws[, lesion_keys, drop = FALSE]
colonoscopy_points <- setNames(
  colonoscopy_parameters$estimate,
  colonoscopy_parameters$lesion_key
)[lesion_keys]

if (any(!is.finite(fit_points)) || any(fit_points <= 0 | fit_points > 1) ||
    any(!is.finite(fit_draws)) || any(fit_draws <= 0 | fit_draws > 1) ||
    any(!is.finite(colonoscopy_points)) ||
    any(colonoscopy_points <= 0 | colonoscopy_points > 1) ||
    any(!is.finite(colonoscopy_draws)) ||
    any(colonoscopy_draws <= 0 | colonoscopy_draws > 1)) {
  stop("Sensitivity point estimates or draws are invalid.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 6. Four uncertainty runs and plug-in estimates
# -----------------------------------------------------------------------------

run_draw_dimensions <- c(
  n_bootstrap,
  length(assignments),
  length(populations),
  length(estimands),
  length(lesion_keys),
  length(fit_scenarios),
  length(uncertainty_runs)
)
run_draw_dimnames <- list(
  NULL,
  assignments,
  populations,
  estimands,
  lesion_keys,
  fit_scenarios,
  uncertainty_runs
)
names(run_draw_dimnames) <- c(
  "Replicate", "Assignment", "Population", "Estimand", "Lesion",
  "FIT_scenario", "Uncertainty_run"
)
run_draws <- array(
  NA_real_,
  dim = run_draw_dimensions,
  dimnames = run_draw_dimnames
)

summary_rows <- list()
summary_row_index <- 0L

for (assignment in assignments) {
  for (population in populations) {
    for (lesion in lesion_keys) {
      q_observed <- q_plugin[assignment, population, lesion]
      q_sampled <- q_bootstrap[, assignment, population, lesion]
      for (fit_scenario in fit_scenarios) {
        fit_point <- fit_points[fit_scenario, lesion]
        fit_sampled <- fit_draws[, fit_scenario, lesion]
        colonoscopy_point <- colonoscopy_points[[lesion]]
        colonoscopy_sampled <- colonoscopy_draws[, lesion]

        for (estimand in estimands) {
          plugin_estimate <- q_observed / fit_point
          if (estimand == "Prevalence") {
            plugin_estimate <- plugin_estimate / colonoscopy_point
          }

          for (uncertainty_run in uncertainty_runs) {
            q_values <- if (uncertainty_run %in%
                            c("Empirical data only", "Full analysis")) {
              q_sampled
            } else {
              rep.int(q_observed, n_bootstrap)
            }
            fit_values <- if (uncertainty_run %in%
                              c("FIT sensitivity only", "Full analysis")) {
              fit_sampled
            } else {
              rep.int(fit_point, n_bootstrap)
            }
            colonoscopy_values <- if (
              estimand == "Prevalence" &&
                uncertainty_run %in%
                  c("Colonoscopy sensitivity only", "Full analysis")
            ) {
              colonoscopy_sampled
            } else {
              rep.int(colonoscopy_point, n_bootstrap)
            }

            values <- q_values / fit_values
            if (estimand == "Prevalence") {
              values <- values / colonoscopy_values
            }
            if (any(!is.finite(values)) || any(values < 0)) {
              stop("A requested uncertainty run produced invalid estimates.", call. = FALSE)
            }

            run_draws[
              , assignment, population, estimand, lesion,
              fit_scenario, uncertainty_run
            ] <- values
            interval <- quantile(
              values,
              probs = c(0.025, 0.975),
              names = FALSE,
              type = 7
            )
            summary_row_index <- summary_row_index + 1L
            summary_rows[[summary_row_index]] <- data.frame(
              Assignment = assignment,
              Population = population,
              Estimand = estimand,
              Lesion_key = lesion,
              Lesion = unname(lesion_labels[[lesion]]),
              FIT_sensitivity_scenario = fit_scenario,
              Uncertainty_run = uncertainty_run,
              Plugin_estimate = plugin_estimate,
              Simulation_mean = mean(values),
              Simulation_SD = sd(values),
              Percentile_lower_95 = interval[[1]],
              Percentile_upper_95 = interval[[2]],
              Probability_above_1 = mean(values > 1),
              N_bootstrap = n_bootstrap,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
}

run_summaries <- do.call(rbind, summary_rows)
rownames(run_summaries) <- NULL

expected_summary_rows <- length(assignments) * length(populations) *
  length(estimands) * length(lesion_keys) * length(fit_scenarios) *
  length(uncertainty_runs)
if (nrow(run_summaries) != expected_summary_rows ||
    any(!is.finite(as.matrix(run_summaries[, c(
      "Plugin_estimate", "Simulation_mean", "Simulation_SD",
      "Percentile_lower_95", "Percentile_upper_95",
      "Probability_above_1"
    )]))) ||
    any(run_summaries$Percentile_lower_95 >
        run_summaries$Percentile_upper_95)) {
  stop("Run-summary validation failed.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 7. Exact log-scale variance allocation for independent input blocks
# -----------------------------------------------------------------------------

variance_rows <- list()
variance_row_index <- 0L

for (assignment in assignments) {
  for (population in populations) {
    for (lesion in lesion_keys) {
      data_log_variance <- var(log(q_bootstrap[, assignment, population, lesion]))
      for (fit_scenario in fit_scenarios) {
        fit_log_variance <- var(log(fit_draws[, fit_scenario, lesion]))
        colonoscopy_log_variance <- var(log(colonoscopy_draws[, lesion]))
        for (estimand in estimands) {
          colonoscopy_component <- if (
            estimand == "Prevalence"
          ) colonoscopy_log_variance else 0
          component_sum <- data_log_variance + fit_log_variance +
            colonoscopy_component
          full_values <- run_draws[
            , assignment, population, estimand, lesion,
            fit_scenario, "Full analysis"
          ]
          empirical_full_log_variance <- var(log(full_values))

          variance_row_index <- variance_row_index + 1L
          variance_rows[[variance_row_index]] <- data.frame(
            Assignment = assignment,
            Population = population,
            Estimand = estimand,
            Lesion_key = lesion,
            Lesion = unname(lesion_labels[[lesion]]),
            FIT_sensitivity_scenario = fit_scenario,
            Data_log_variance = data_log_variance,
            FIT_log_variance = fit_log_variance,
            Colonoscopy_log_variance = colonoscopy_component,
            Component_log_variance_sum = component_sum,
            Full_log_variance_empirical = empirical_full_log_variance,
            Monte_Carlo_covariance_difference =
              empirical_full_log_variance - component_sum,
            Data_variance_share = data_log_variance / component_sum,
            FIT_variance_share = fit_log_variance / component_sum,
            Colonoscopy_variance_share =
              colonoscopy_component / component_sum,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
}

variance_shares <- do.call(rbind, variance_rows)
rownames(variance_shares) <- NULL
share_sum <- rowSums(variance_shares[, c(
  "Data_variance_share", "FIT_variance_share",
  "Colonoscopy_variance_share"
)])
if (any(!is.finite(as.matrix(variance_shares[, c(
      "Data_log_variance", "FIT_log_variance",
      "Colonoscopy_log_variance", "Component_log_variance_sum",
      "Full_log_variance_empirical", "Monte_Carlo_covariance_difference",
      "Data_variance_share", "FIT_variance_share",
      "Colonoscopy_variance_share"
    )]))) ||
    any(abs(share_sum - 1) > 1e-10)) {
  stop("Log-scale variance allocation failed.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 8. Metadata and export
# -----------------------------------------------------------------------------

metadata <- data.frame(
  Parameter = c(
    "method_version",
    "analysis_population",
    "participant_frame",
    "participant_frame_md5",
    "sensitivity_cohort",
    "external_fit_parameters",
    "colonoscopy_parameters",
    "participant_n",
    "men_n",
    "women_n",
    "bootstrap_replicates",
    "bootstrap_unit",
    "bootstrap_strata",
    "bootstrap_engine",
    "joint_resampling",
    "empirical_component",
    "point_estimate",
    "interval",
    "total_population_aggregation",
    "local_cancer_fit_uncertainty",
    "probability_bounds",
    "seed_sensitivity_selection",
    "seed_bootstrap",
    "seed_local_fit",
    "seed_literature_fit",
    "seed_colonoscopy",
    "R_version",
    "readxl_version"
  ),
  Value = c(
    "joint-participant-bootstrap-v1",
    "Average-risk PRENEC participants aged 50-75 with at least one observed FIT",
    basename(normalizePath(
      bootstrap_frame_file,
      winslash = "/",
      mustWork = TRUE
    )),
    unname(tools::md5sum(bootstrap_frame_file)),
    basename(normalizePath(
      sensitivity_cohort_file,
      winslash = "/",
      mustWork = TRUE
    )),
    basename(normalizePath(
      external_fit_parameter_file,
      winslash = "/",
      mustWork = TRUE
    )),
    basename(normalizePath(
      colonoscopy_parameter_file,
      winslash = "/",
      mustWork = TRUE
    )),
    as.character(nrow(participant_frame)),
    as.character(sum(participant_frame$Sex == "Men")),
    as.character(sum(participant_frame$Sex == "Women")),
    as.character(n_bootstrap),
    "Participant",
    "Sex x age group (50-54, 55-59, 60-64, 65-69, 70-74, 75)",
    "Multinomial counts over observed joint participant patterns within stratum",
    "One bootstrap bank reused across A/B, lesions, populations, and FIT scenarios",
    "sum_h (N_h/N)(F_h/N_h)(D_jh/C_h)",
    "Observed-count and point-sensitivity plug-in estimate",
    "Equal-tail percentile 95% interval (R quantile type 7)",
    "Sex-by-age strata aggregated with fixed analytical-population weights",
    "Jeffreys Beta(6.5,0.5) from TP=6 and FN=0; deliberate change from fixed 1.0",
    "Simulation draws are not truncated; Probability_above_1 is exported",
    as.character(seed_sensitivity_selection),
    as.character(seed_bootstrap),
    as.character(seed_local_fit),
    as.character(seed_literature_fit),
    as.character(seed_colonoscopy),
    R.version.string,
    as.character(utils::packageVersion("readxl"))
  ),
  stringsAsFactors = FALSE
)

local_fit_export <- local_fit_parameters
local_fit_export$Scenario <- "PRENEC"
external_fit_export <- data.frame(
  Lesion_key = external_fit_parameters$lesion_key,
  Lesion = unname(lesion_labels[external_fit_parameters$lesion_key]),
  Estimate = external_fit_parameters$estimate,
  Distribution = external_fit_parameters$distribution,
  Beta_a = external_fit_parameters$beta_a,
  Beta_b = external_fit_parameters$beta_b,
  Logit_mean = external_fit_parameters$logit_mean,
  Logit_se = external_fit_parameters$logit_se,
  Prior_added = external_fit_parameters$prior_added_in_simulation,
  Source_ID = external_fit_parameters$source_id,
  Scenario = "Literature",
  stringsAsFactors = FALSE
)
colonoscopy_export <- data.frame(
  Lesion_key = colonoscopy_parameters$lesion_key,
  Lesion = unname(lesion_labels[colonoscopy_parameters$lesion_key]),
  Estimate = colonoscopy_parameters$estimate,
  Beta_a = colonoscopy_parameters$beta_a,
  Beta_b = colonoscopy_parameters$beta_b,
  Prior_added = colonoscopy_parameters$prior_added_in_R,
  Source_ID = colonoscopy_parameters$source_id,
  stringsAsFactors = FALSE
)

write_tsv(run_summaries, "joint_bootstrap_run_summaries.tsv")
write_tsv(variance_shares, "joint_bootstrap_log_variance_shares.tsv")
write_tsv(observed_counts, "joint_bootstrap_observed_counts.tsv")
write_tsv(metadata, "joint_bootstrap_metadata.tsv")
write_tsv(local_fit_export, "joint_bootstrap_local_fit_parameters.tsv")
write_tsv(external_fit_export, "joint_bootstrap_literature_fit_parameters.tsv")
write_tsv(colonoscopy_export, "joint_bootstrap_colonoscopy_parameters.tsv")

result_object <- list(
  metadata = metadata,
  observed_joint_counts = observed_counts,
  plugin_empirical_component = q_plugin,
  bootstrap_empirical_component = q_bootstrap,
  fit_points = fit_points,
  fit_draws = fit_draws,
  colonoscopy_points = colonoscopy_points,
  colonoscopy_draws = colonoscopy_draws,
  run_draws = run_draws,
  run_summaries = run_summaries,
  log_variance_shares = variance_shares,
  local_fit_parameters = local_fit_export,
  literature_fit_parameters = external_fit_export,
  colonoscopy_parameters = colonoscopy_export
)
saveRDS(
  result_object,
  file = file.path(output_directory, "joint_bootstrap_uncertainty.rds"),
  version = 3
)

message(
  "Joint participant-bootstrap uncertainty analysis completed: ",
  normalizePath(output_directory, winslash = "/", mustWork = TRUE)
)

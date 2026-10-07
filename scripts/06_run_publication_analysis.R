# -*- coding: UTF-8 -*-

# Stage 06: PRENEC publication model ------------------------------------------
# PRENEC: adjusted population detection and modelled prevalence
# -----------------------------------------------------------------------------
# Outcomes:
#   - low-risk adenoma;
#   - high-risk adenoma;
#   - colorectal cancer.
#
# All outcomes are defined at the patient level. For lesion j, age e and
# simulation m:
#
#   d[j,e,m] = simulated observed programme detection rate
#   r[j,e,m] = d[j,e,m] / (SeFIT[j,m] * Adh[e,m])
#   p[j,e,m] = r[j,e,m] / SeCOL[j,m]
#
# Here, SeFIT is FIT sensitivity, Adh is colonoscopy adherence conditional on a
# positive selected FIT, and SeCOL is colonoscopy sensitivity. The resulting r
# is the adjusted population detection rate and p is modelled prevalence.
# Age-specific estimates are weighted by the corresponding population count.
# The analysis is performed for databases A and B, for both sexes combined and
# separately for men and women.
#
# The population variable (pop) remains the denominator for D and the age
# weights. FIT-positive and colonoscopy counts are used only to estimate Adh,
# defined as colonoscopy completion within the programme window conditional on
# a positive selected FIT. The model does not correct for FIT coverage or FIT
# positivity, and test specificity does not enter either equation.
#
# Uncertainty is propagated jointly through Monte Carlo sampling. Programme
# detection, FIT sensitivity, adherence, and colonoscopy sensitivity are drawn
# in each iteration according to the parameterisations documented below. A
# common random-number design is used across databases and FIT-sensitivity
# scenarios to support direct paired comparisons.
#
# The script can be run from any working directory. Input and output paths are
# resolved from the location of this file unless an optional PRENEC_* environment
# variable is supplied. All exported tables use one row per analysis quantity.

suppressPackageStartupMessages({
  library(coda)
  library(dplyr)
  library(readxl)
})

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Reproducible configuration
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

seed_general <- Sys.getenv("PRENEC_SEED", unset = "20260923")
seed_bases <- Sys.getenv("PRENEC_BASE_SEED", unset = seed_general)
seed_sensitivity <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_SENS",
  unset = seed_general
)))
seed_model <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_MODEL",
  unset = "20260924"
)))
seed_colonoscopy <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_COLONOSCOPY",
  unset = as.character(seed_model + 1L)
)))
seed_external_fit <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_FIT_EXTERNAL",
  unset = as.character(seed_model + 2L)
)))
seed_adherence <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_SEED_ADHERENCE",
  unset = as.character(seed_model + 4L)
)))
n_sim <- suppressWarnings(as.integer(Sys.getenv(
  "PRENEC_N_SIM",
  unset = "10000"
)))

numeric_configuration <- c(
  seed_sensitivity,
  seed_model,
  seed_colonoscopy,
  seed_external_fit,
  seed_adherence,
  n_sim
)
if (anyNA(numeric_configuration) || n_sim <= 0L) {
  stop("Simulation seeds and PRENEC_N_SIM must be valid integers.", call. = FALSE)
}

base_directory <- Sys.getenv(
  "PRENEC_BASE_DIR",
  unset = file.path(project_directory, "data", "intermediate")
)
output_directory <- Sys.getenv(
  "PRENEC_PUBLICATION_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "publication")
)
colonoscopy_parameter_file <- Sys.getenv(
  "PRENEC_COLON_PARAMS",
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
    "external_fit_sensitivity_parameters.xlsx"
  )
)
sensitivity_cohort_file <- Sys.getenv(
  "PRENEC_BASE_SENS",
  unset = file.path(project_directory, "data", "private", "Base_prenec.xlsx")
)

required_files <- c(
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

lesion_keys <- c("low_risk", "high_risk", "cancer")
lesion_labels <- c(
  low_risk = "Low-risk adenoma",
  high_risk = "High-risk adenoma",
  cancer = "Colorectal cancer"
)
count_columns <- c(
  low_risk = "low_risk",
  high_risk = "high_risk",
  cancer = "cancer"
)
age_groups <- c("50-54", "55-59", "60-64", "65-69", "70-74", "75")
fit_source_labels <- c(
  PRENEC = "PRENEC-derived FIT sensitivity",
  Literature = "Literature-based FIT sensitivity"
)

normalise_logical <- function(x, field) {
  if (is.logical(x)) {
    if (anyNA(x)) {
      stop(field, " contains missing logical values.", call. = FALSE)
    }
    return(x)
  }
  text <- tolower(trimws(as.character(x)))
  result <- rep(NA, length(text))
  result[text %in% c("true", "t", "1", "yes", "si")] <- TRUE
  result[text %in% c("false", "f", "0", "no")] <- FALSE
  if (anyNA(result)) {
    stop(field, " contains unrecognised logical values.", call. = FALSE)
  }
  result
}

age_group <- function(age) {
  ifelse(
    age >= 50 & age <= 54, "50-54",
    ifelse(
      age >= 55 & age <= 59, "55-59",
      ifelse(
        age >= 60 & age <= 64, "60-64",
        ifelse(
          age >= 65 & age <= 69, "65-69",
          ifelse(age >= 70 & age <= 74, "70-74", ifelse(age == 75, "75", NA_character_))
        )
      )
    )
  )
}

wilson_interval <- function(successes, total, confidence = 0.95) {
  if (length(successes) != 1L || length(total) != 1L ||
      !is.finite(successes) || !is.finite(total) ||
      total <= 0 || successes < 0 || successes > total) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  z <- qnorm(1 - (1 - confidence) / 2)
  p <- successes / total
  denominator <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt(
    p * (1 - p) / total + z^2 / (4 * total^2)
  ) / denominator
  c(
    lower = max(0, centre - half_width),
    upper = min(1, centre + half_width)
  )
}

hpd_summary <- function(x, probability = 0.95) {
  if (length(x) == 0L || any(!is.finite(x))) {
    stop("Simulation draws must be finite and non-empty.", call. = FALSE)
  }
  interval <- coda::HPDinterval(coda::mcmc(x), prob = probability)
  c(
    mean = mean(x),
    lower = as.numeric(interval[1]),
    upper = as.numeric(interval[2])
  )
}

# -----------------------------------------------------------------------------
# 2. Patient-level FIT sensitivity
# -----------------------------------------------------------------------------

# A single observed FIT is selected for each high-risk participant. When two
# results are available, FIT 1 or FIT 2 is selected with equal probability;
# when only one is available, that result is retained. Selection is performed
# once before the prevalence simulation and remains fixed across iterations.
sensitivity_data <- as.data.frame(readxl::read_excel(
  sensitivity_cohort_file
))
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

sensitivity_data <- sensitivity_data[
  sensitivity_data$highrisk == 1,
  required_sensitivity_columns,
  drop = FALSE
]
if (nrow(sensitivity_data) == 0L ||
    anyNA(sensitivity_data$Codigo) ||
    anyDuplicated(sensitivity_data$Codigo)) {
  stop(
    "The PRENEC high-risk sensitivity cohort requires one row per participant.",
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

sensitivity_data <- sensitivity_data %>%
  mutate(
    Tamano_mm = suppressWarnings(as.numeric(Tamano_mm)),
    adenoma_present = as.integer(Tipo_Histologico_1 %in% adenoma_histologies),
    adenoma_risk = case_when(
      Tipo_Histologico_1 == adenoma_histologies[[1]] & Tamano_mm >= 10 ~ 1,
      Tipo_Histologico_1 %in% adenoma_histologies[-1] ~ 1,
      Tipo_Histologico_1 == adenoma_histologies[[1]] & Tamano_mm < 10 ~ 0,
      TRUE ~ NA_real_
    ),
    cancer_present = as.integer(
      Tipo_Histologico_1 %in% invasive_cancer_histologies
    ),
    FIT_1 = case_when(
      is.na(Valor_TSDO_1_1) ~ NA_character_,
      Valor_TSDO_1_1 >= 100 ~ "Positive",
      TRUE ~ "Negative"
    ),
    FIT_2 = case_when(
      is.na(Valor_TSDO_2_1) ~ NA_character_,
      Valor_TSDO_2_1 >= 100 ~ "Positive",
      TRUE ~ "Negative"
    )
  )

# Sorting by participant identifier makes the random selection independent of
# row order while retaining the prespecified random-number stream.
participant_order <- order(
  as.character(sensitivity_data$Codigo),
  method = "radix"
)
RNGkind(
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)
set.seed(seed_sensitivity)
selection_uniform <- numeric(nrow(sensitivity_data))
selection_uniform[participant_order] <- runif(nrow(sensitivity_data))

sensitivity_data <- sensitivity_data %>%
  mutate(
    selected_test = case_when(
      !is.na(FIT_1) & !is.na(FIT_2) & selection_uniform < 0.5 ~ 1L,
      !is.na(FIT_1) & !is.na(FIT_2) ~ 2L,
      !is.na(FIT_1) ~ 1L,
      !is.na(FIT_2) ~ 2L,
      TRUE ~ NA_integer_
    ),
    selected_FIT = case_when(
      selected_test == 1L ~ FIT_1,
      selected_test == 2L ~ FIT_2,
      TRUE ~ NA_character_
    )
  )

lesion_status <- list(
  low_risk = case_when(
    sensitivity_data$adenoma_present == 1 &
      sensitivity_data$adenoma_risk == 0 ~ 1L,
    sensitivity_data$adenoma_present == 0 ~ 0L,
    sensitivity_data$adenoma_present == 1 &
      !is.na(sensitivity_data$adenoma_risk) ~ 0L,
    TRUE ~ NA_integer_
  ),
  high_risk = case_when(
    sensitivity_data$adenoma_present == 1 &
      sensitivity_data$adenoma_risk == 1 ~ 1L,
    sensitivity_data$adenoma_present == 0 ~ 0L,
    sensitivity_data$adenoma_present == 1 &
      !is.na(sensitivity_data$adenoma_risk) ~ 0L,
    TRUE ~ NA_integer_
  ),
  cancer = sensitivity_data$cancer_present
)

local_fit_rows <- lapply(lesion_keys, function(key) {
  cases <- lesion_status[[key]] == 1L
  cases[is.na(cases)] <- FALSE
  included <- cases & !is.na(sensitivity_data$selected_FIT)
  total <- sum(included)
  positive <- sum(included & sensitivity_data$selected_FIT == "Positive")
  interval <- wilson_interval(positive, total)
  data.frame(
    lesion_key = key,
    true_positive = positive,
    false_negative = total - positive,
    estimate = positive / total,
    lower = interval[["lower"]],
    upper = interval[["upper"]],
    stringsAsFactors = FALSE
  )
})
local_fit_parameters <- do.call(rbind, local_fit_rows)
rownames(local_fit_parameters) <- NULL

if (anyNA(local_fit_parameters) ||
    any(local_fit_parameters$true_positive < 0) ||
    any(local_fit_parameters$false_negative < 0)) {
  stop("PRENEC FIT sensitivity parameters are incomplete.", call. = FALSE)
}

local_fit_table <- data.frame(
  Category = unname(lesion_labels[local_fit_parameters$lesion_key]),
  Mean = local_fit_parameters$estimate,
  Lower_CI = local_fit_parameters$lower,
  Upper_CI = local_fit_parameters$upper,
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 3. External FIT and colonoscopy sensitivity
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
      "External FIT parameter table is missing: ",
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
             table$lesion_key %in% c("high_risk", "cancer"))
      ),
    ,
    drop = FALSE
  ]
  expected <- c(
    "meta_naa_oc100::low_risk",
    "lin_2021_oc_sensor::high_risk",
    "lin_2021_oc_sensor::cancer"
  )
  observed <- paste(table$scenario_id, table$lesion_key, sep = "::")
  if (!setequal(observed, expected) || anyDuplicated(observed)) {
    stop("The three required external FIT inputs are not uniquely defined.", call. = FALSE)
  }
  numeric_fields <- c(
    "estimate", "ci_lower", "ci_upper", "beta_a", "beta_b",
    "logit_mean", "logit_se", "prior_added_in_simulation"
  )
  for (field in numeric_fields) {
    table[[field]] <- suppressWarnings(as.numeric(table[[field]]))
  }
  if (any(!table$distribution %in% c("beta", "logit_normal")) ||
      any(table$estimate <= 0 | table$estimate >= 1)) {
    stop("External FIT distributions are not valid.", call. = FALSE)
  }
  table
}

draw_external_fit_scenario <- function(parameters, order, n, seed) {
  parameters <- parameters[match(order, parameters$lesion_key), , drop = FALSE]
  if (anyNA(parameters$lesion_key)) {
    stop("External FIT scenario is incomplete.", call. = FALSE)
  }
  set.seed(seed)
  uniforms <- matrix(
    runif(n * nrow(parameters)),
    nrow = n,
    ncol = nrow(parameters)
  )
  draws <- vapply(seq_len(nrow(parameters)), function(j) {
    if (parameters$distribution[[j]] == "logit_normal") {
      if (!is.finite(parameters$logit_mean[[j]]) ||
          !is.finite(parameters$logit_se[[j]]) ||
          parameters$logit_se[[j]] <= 0) {
        stop("Logit-normal FIT parameters are incomplete.", call. = FALSE)
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
      stop("Beta FIT parameters are incomplete.", call. = FALSE)
    }
    qbeta(uniforms[, j], a, b)
  }, numeric(n))
  colnames(draws) <- order
  draws
}

external_fit_parameters <- read_external_fit_parameters(
  external_fit_parameter_file
)
external_fit_low <- draw_external_fit_scenario(
  external_fit_parameters[
    external_fit_parameters$scenario_id == "meta_naa_oc100",
    ,
    drop = FALSE
  ],
  "low_risk",
  n_sim,
  seed_external_fit
)
external_fit_high_cancer <- draw_external_fit_scenario(
  external_fit_parameters[
    external_fit_parameters$scenario_id == "lin_2021_oc_sensor",
    ,
    drop = FALSE
  ],
  c("high_risk", "cancer"),
  n_sim,
  seed_external_fit
)
external_fit_draws <- cbind(
  low_risk = external_fit_low[, "low_risk"],
  high_risk = external_fit_high_cancer[, "high_risk"],
  cancer = external_fit_high_cancer[, "cancer"]
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
      "Colonoscopy parameter table is missing: ",
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
    stop("Colonoscopy sensitivity parameters are not valid.", call. = FALSE)
  }
  table
}

colonoscopy_parameters <- read_colonoscopy_parameters(
  colonoscopy_parameter_file
)

# The parameter stream contains seven fixed positions. Positions 4-6 correspond
# to the three reported outcomes and remain unchanged across analysis versions.
set.seed(seed_colonoscopy)
colonoscopy_uniforms <- matrix(
  runif(n_sim * 7L),
  nrow = n_sim,
  ncol = 7L
)
colonoscopy_draws <- vapply(seq_along(lesion_keys), function(j) {
  key <- lesion_keys[[j]]
  row <- colonoscopy_parameters[
    colonoscopy_parameters$lesion_key == key,
    ,
    drop = FALSE
  ]
  qbeta(
    colonoscopy_uniforms[, j + 3L],
    row$beta_a[[1]] + row$prior_added_in_R[[1]],
    row$beta_b[[1]] + row$prior_added_in_R[[1]]
  )
}, numeric(n_sim))
colnames(colonoscopy_draws) <- lesion_keys

if (any(!is.finite(external_fit_draws)) ||
    any(external_fit_draws <= 0 | external_fit_draws > 1) ||
    any(!is.finite(colonoscopy_draws)) ||
    any(colonoscopy_draws <= 0 | colonoscopy_draws > 1)) {
  stop("Sensitivity draws must be finite and lie in (0, 1].", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 4. Analysis datasets and adherence parameters
# -----------------------------------------------------------------------------

analysis_files <- data.frame(
  Analysis = c(
    "A_Total", "A_Men", "A_Women",
    "B_Total", "B_Men", "B_Women"
  ),
  Base = rep(c("A", "B"), each = 3L),
  Sex = rep(c("Both sexes", "Men", "Women"), times = 2L),
  file_sex = rep(c("", "_hombres", "_mujeres"), times = 2L),
  stringsAsFactors = FALSE
)
analysis_files$Path <- file.path(
  base_directory,
  paste0(
    "Base_final", analysis_files$Base,
    "_N_aleatoria", analysis_files$file_sex,
    "_cohorte_actual_semilla_", seed_bases, ".xlsx"
  )
)
missing_bases <- analysis_files$Path[!file.exists(analysis_files$Path)]
if (length(missing_bases) > 0L) {
  stop(
    "Analysis datasets were not found:\n",
    paste(missing_bases, collapse = "\n"),
    call. = FALSE
  )
}

required_base_columns <- c(
  "Edad_2", "pop", "low_risk", "high_risk", "cancer",
  "fit_positive_n", "colonoscopy_n"
)

read_analysis_base <- function(path) {
  data <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  missing <- setdiff(required_base_columns, names(data))
  if (length(missing) > 0L) {
    stop(
      basename(path), " is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  data <- data[, required_base_columns, drop = FALSE]
  for (field in required_base_columns) {
    data[[field]] <- suppressWarnings(as.numeric(data[[field]]))
  }
  data <- data[
    !is.na(data$Edad_2) & data$Edad_2 >= 50 & data$Edad_2 <= 75,
    ,
    drop = FALSE
  ]
  for (field in c("low_risk", "high_risk", "cancer")) {
    data[[field]][is.na(data[[field]])] <- 0
  }
  if (nrow(data) == 0L ||
      any(!is.finite(as.matrix(data))) ||
      any(data$pop < 0) ||
      any(data$fit_positive_n < 0) ||
      any(data$colonoscopy_n < 0) ||
      any(data$colonoscopy_n > data$fit_positive_n) ||
      any(data$fit_positive_n > data$pop)) {
    stop("Invalid denominator counts in ", basename(path), ".", call. = FALSE)
  }
  for (field in c("low_risk", "high_risk", "cancer")) {
    if (any(data[[field]] < 0) ||
        any(data[[field]] > data$colonoscopy_n) ||
        any(data[[field]] > data$pop)) {
      stop("Invalid lesion counts in ", basename(path), ".", call. = FALSE)
    }
  }
  data
}

analysis_bases <- lapply(analysis_files$Path, read_analysis_base)
names(analysis_bases) <- analysis_files$Analysis

validate_sex_totals <- function(base_id) {
  total <- analysis_bases[[paste0(base_id, "_Total")]]
  men <- analysis_bases[[paste0(base_id, "_Men")]]
  women <- analysis_bases[[paste0(base_id, "_Women")]]
  fields <- c(
    "pop", "low_risk", "high_risk", "cancer",
    "fit_positive_n", "colonoscopy_n"
  )
  sum_by_age <- function(x) {
    x %>%
      group_by(Edad_2) %>%
      summarise(across(all_of(fields), sum), .groups = "drop")
  }
  total <- sum_by_age(total)
  sex_sum <- bind_rows(men, women) %>% sum_by_age()
  check <- merge(total, sex_sum, by = "Edad_2", suffixes = c("_total", "_sex"), all = TRUE)
  check[is.na(check)] <- 0
  differences <- vapply(fields, function(field) {
    max(abs(
      check[[paste0(field, "_total")]] -
        check[[paste0(field, "_sex")]]
    ))
  }, numeric(1))
  if (any(differences != 0)) {
    stop(
      "Sex-specific counts do not reconcile for database ", base_id, ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_sex_totals("A")
validate_sex_totals("B")

build_adherence_table <- function(data, analysis_name) {
  context <- analysis_files[
    analysis_files$Analysis == analysis_name,
    ,
    drop = FALSE
  ]
  table <- data %>%
    mutate(Age_group = age_group(Edad_2)) %>%
    group_by(Age_group) %>%
    summarise(
      FIT_positive = sum(fit_positive_n),
      Colonoscopy_window = sum(colonoscopy_n),
      .groups = "drop"
    )
  table <- table[match(age_groups, table$Age_group), , drop = FALSE]
  if (anyNA(table$Age_group) ||
      any(table$FIT_positive <= 0) ||
      any(table$Colonoscopy_window < 0) ||
      any(table$Colonoscopy_window > table$FIT_positive)) {
    stop("Adherence counts are incomplete for ", analysis_name, ".", call. = FALSE)
  }
  intervals <- t(vapply(seq_len(nrow(table)), function(i) {
    wilson_interval(table$Colonoscopy_window[[i]], table$FIT_positive[[i]])
  }, numeric(2)))
  table$Analysis <- analysis_name
  table$Base <- context$Base
  table$Sex <- context$Sex
  table$Adherence <- table$Colonoscopy_window / table$FIT_positive
  table$Wilson_lower <- intervals[, "lower"]
  table$Wilson_upper <- intervals[, "upper"]
  table$Beta_a <- table$Colonoscopy_window + 0.5
  table$Beta_b <- table$FIT_positive - table$Colonoscopy_window + 0.5
  table[, c(
    "Analysis", "Base", "Sex", "Age_group",
    "FIT_positive", "Colonoscopy_window", "Adherence",
    "Wilson_lower", "Wilson_upper", "Beta_a", "Beta_b"
  )]
}

adherence_by_analysis <- lapply(names(analysis_bases), function(name) {
  build_adherence_table(analysis_bases[[name]], name)
})
names(adherence_by_analysis) <- names(analysis_bases)
adherence_table <- do.call(rbind, adherence_by_analysis)
rownames(adherence_table) <- NULL

# A separate random-number stream is used for adherence. The same quantiles are
# reused across lesions and FIT-sensitivity scenarios; each analysis applies its
# own observed colonoscopy and FIT-positive counts.
set.seed(seed_adherence)
adherence_uniforms <- matrix(
  runif(n_sim * length(age_groups)),
  nrow = n_sim,
  ncol = length(age_groups),
  dimnames = list(NULL, age_groups)
)

make_adherence_draws <- function(data, table) {
  table <- table[match(age_groups, table$Age_group), , drop = FALSE]
  group_draws <- vapply(seq_along(age_groups), function(j) {
    qbeta(
      adherence_uniforms[, j],
      table$Beta_a[[j]],
      table$Beta_b[[j]]
    )
  }, numeric(n_sim))
  colnames(group_draws) <- age_groups
  index <- match(age_group(data$Edad_2), age_groups)
  if (anyNA(index)) {
    stop("An age could not be assigned to an adherence group.", call. = FALSE)
  }
  group_draws[, index, drop = FALSE]
}

adherence_draws <- Map(
  make_adherence_draws,
  analysis_bases,
  adherence_by_analysis
)

if (any(!vapply(adherence_draws, function(x) {
  all(is.finite(x)) && all(x > 0 & x < 1)
}, logical(1)))) {
  stop("Adherence draws must be finite and lie in (0, 1).", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 5. Monte Carlo simulation
# -----------------------------------------------------------------------------

simulate_analysis <- function(data, adherence, analysis_name) {
  population <- data$pop
  total_population <- sum(population)
  if (!is.finite(total_population) || total_population <= 0) {
    stop("Population is not valid for ", analysis_name, ".", call. = FALSE)
  }

  adjusted <- lapply(fit_source_labels, function(x) {
    matrix(
      NA_real_,
      nrow = n_sim,
      ncol = length(lesion_keys),
      dimnames = list(NULL, lesion_keys)
    )
  })
  prevalence <- lapply(fit_source_labels, function(x) {
    matrix(
      NA_real_,
      nrow = n_sim,
      ncol = length(lesion_keys),
      dimnames = list(NULL, lesion_keys)
    )
  })

  for (i in seq_len(n_sim)) {
    # Fixed-width streams keep the retained outcomes reproducible if the list
    # of reported outcomes changes. The selected positions correspond to the
    # three prespecified lesion definitions.
    u_detection <- runif(6)
    u_local_fit <- runif(5)
    invisible(runif(1))

    detection_by_age <- cbind(
      low_risk = suppressWarnings(qbeta(
        u_detection[[4]],
        data$low_risk,
        data$pop - data$low_risk
      )),
      high_risk = suppressWarnings(qbeta(
        u_detection[[5]],
        data$high_risk,
        data$pop - data$high_risk
      )),
      cancer = suppressWarnings(qbeta(
        u_detection[[6]],
        data$cancer,
        data$pop - data$cancer
      ))
    )

    local_fit <- c(
      low_risk = qbeta(
        u_local_fit[[4]],
        local_fit_parameters$true_positive[
          local_fit_parameters$lesion_key == "low_risk"
        ] + 0.5,
        local_fit_parameters$false_negative[
          local_fit_parameters$lesion_key == "low_risk"
        ] + 0.5
      ),
      high_risk = qbeta(
        u_local_fit[[5]],
        local_fit_parameters$true_positive[
          local_fit_parameters$lesion_key == "high_risk"
        ] + 0.5,
        local_fit_parameters$false_negative[
          local_fit_parameters$lesion_key == "high_risk"
        ] + 0.5
      ),
      cancer = local_fit_parameters$estimate[
        local_fit_parameters$lesion_key == "cancer"
      ]
    )
    literature_fit <- external_fit_draws[i, lesion_keys]
    fit_values <- list(
      PRENEC = local_fit,
      Literature = literature_fit
    )

    for (source in names(fit_values)) {
      for (key in lesion_keys) {
        adjusted_by_age <- detection_by_age[, key] / (
          fit_values[[source]][[key]] * adherence[i, ]
        )
        prevalence_by_age <- adjusted_by_age / colonoscopy_draws[i, key]
        adjusted[[source]][i, key] <- sum(
          adjusted_by_age * population,
          na.rm = TRUE
        ) / total_population
        prevalence[[source]][i, key] <- sum(
          prevalence_by_age * population,
          na.rm = TRUE
        ) / total_population
      }
    }
  }

  if (any(!vapply(c(adjusted, prevalence), function(x) {
    all(is.finite(x))
  }, logical(1)))) {
    stop("The simulation produced non-finite results for ", analysis_name, ".", call. = FALSE)
  }

  list(
    adjusted_detection = adjusted,
    modelled_prevalence = prevalence,
    population = total_population
  )
}

# The seed is set once and the six analyses are evaluated in a fixed order.
set.seed(seed_model)
simulation_results <- vector("list", length(analysis_bases))
names(simulation_results) <- names(analysis_bases)
for (name in names(analysis_bases)) {
  message("Simulating ", name, "...")
  simulation_results[[name]] <- simulate_analysis(
    analysis_bases[[name]],
    adherence_draws[[name]],
    name
  )
}

# -----------------------------------------------------------------------------
# 6. Results tables
# -----------------------------------------------------------------------------

summary_rows <- list()
row_index <- 0L
for (analysis_name in names(simulation_results)) {
  context <- analysis_files[
    analysis_files$Analysis == analysis_name,
    ,
    drop = FALSE
  ]
  result <- simulation_results[[analysis_name]]
  outcome_matrices <- list(
    "Adjusted population detection rate" = result$adjusted_detection,
    "Modelled prevalence" = result$modelled_prevalence
  )
  for (outcome in names(outcome_matrices)) {
    for (source in names(outcome_matrices[[outcome]])) {
      matrix_draws <- outcome_matrices[[outcome]][[source]]
      for (key in lesion_keys) {
        interval <- hpd_summary(matrix_draws[, key])
        row_index <- row_index + 1L
        summary_rows[[row_index]] <- data.frame(
          Analysis = analysis_name,
          Base = context$Base,
          Sex = context$Sex,
          Outcome = outcome,
          Lesion_key = key,
          Lesion = unname(lesion_labels[[key]]),
          FIT_sensitivity_source = unname(fit_source_labels[[source]]),
          Mean = interval[["mean"]],
          Lower_95 = interval[["lower"]],
          Upper_95 = interval[["upper"]],
          Mean_percent = 100 * interval[["mean"]],
          Lower_95_percent = 100 * interval[["lower"]],
          Upper_95_percent = 100 * interval[["upper"]],
          Population = result$population,
          Simulations = n_sim,
          Interval = "95% HPD interval",
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
model_estimates <- do.call(rbind, summary_rows)
rownames(model_estimates) <- NULL

colonoscopy_detection_rows <- list()
row_index <- 0L
for (analysis_name in names(analysis_bases)) {
  data <- analysis_bases[[analysis_name]]
  context <- analysis_files[
    analysis_files$Analysis == analysis_name,
    ,
    drop = FALSE
  ]
  colonoscopies <- sum(data$colonoscopy_n)
  population <- sum(data$pop)
  for (key in lesion_keys) {
    cases <- sum(data[[count_columns[[key]]]])
    interval <- wilson_interval(cases, colonoscopies)
    row_index <- row_index + 1L
    colonoscopy_detection_rows[[row_index]] <- data.frame(
      Analysis = analysis_name,
      Base = context$Base,
      Sex = context$Sex,
      Lesion_key = key,
      Lesion = unname(lesion_labels[[key]]),
      Lesion_positive_patients = cases,
      Colonoscopies = colonoscopies,
      Population = population,
      Colonoscopy_detection_rate = cases / colonoscopies,
      Wilson_lower_95 = interval[["lower"]],
      Wilson_upper_95 = interval[["upper"]],
      Observed_program_detection_rate = cases / population,
      stringsAsFactors = FALSE
    )
  }
}
colonoscopy_detection <- do.call(rbind, colonoscopy_detection_rows)
rownames(colonoscopy_detection) <- NULL

adherence_draw_summary <- do.call(rbind, lapply(names(adherence_by_analysis), function(name) {
  table <- adherence_by_analysis[[name]]
  draws <- vapply(seq_along(age_groups), function(j) {
    qbeta(
      adherence_uniforms[, j],
      table$Beta_a[[j]],
      table$Beta_b[[j]]
    )
  }, numeric(n_sim))
  data.frame(
    table,
    Draw_mean = colMeans(draws),
    Draw_lower_2_5 = apply(draws, 2, quantile, 0.025),
    Draw_upper_97_5 = apply(draws, 2, quantile, 0.975),
    stringsAsFactors = FALSE
  )
}))
rownames(adherence_draw_summary) <- NULL

fit_input_table <- rbind(
  data.frame(
    Test = "FIT",
    Source = fit_source_labels[["PRENEC"]],
    Lesion_key = lesion_keys,
    Lesion = unname(lesion_labels),
    Estimate = local_fit_table$Mean,
    Lower_95 = local_fit_table$Lower_CI,
    Upper_95 = local_fit_table$Upper_CI,
    Distribution = c("beta", "beta", "fixed"),
    Source_ID = "PRENEC high-risk cohort",
    stringsAsFactors = FALSE
  ),
  data.frame(
    Test = "FIT",
    Source = fit_source_labels[["Literature"]],
    Lesion_key = lesion_keys,
    Lesion = unname(lesion_labels),
    Estimate = external_fit_parameters$estimate[
      match(lesion_keys, external_fit_parameters$lesion_key)
    ],
    Lower_95 = external_fit_parameters$ci_lower[
      match(lesion_keys, external_fit_parameters$lesion_key)
    ],
    Upper_95 = external_fit_parameters$ci_upper[
      match(lesion_keys, external_fit_parameters$lesion_key)
    ],
    Distribution = external_fit_parameters$distribution[
      match(lesion_keys, external_fit_parameters$lesion_key)
    ],
    Source_ID = external_fit_parameters$source_id[
      match(lesion_keys, external_fit_parameters$lesion_key)
    ],
    stringsAsFactors = FALSE
  )
)

colonoscopy_input_table <- data.frame(
  Test = "Colonoscopy",
  Source = "External evidence",
  Lesion_key = colonoscopy_parameters$lesion_key,
  Lesion = unname(lesion_labels[colonoscopy_parameters$lesion_key]),
  Estimate = colonoscopy_parameters$estimate,
  Lower_95 = qbeta(
    0.025,
    colonoscopy_parameters$beta_a + colonoscopy_parameters$prior_added_in_R,
    colonoscopy_parameters$beta_b + colonoscopy_parameters$prior_added_in_R
  ),
  Upper_95 = qbeta(
    0.975,
    colonoscopy_parameters$beta_a + colonoscopy_parameters$prior_added_in_R,
    colonoscopy_parameters$beta_b + colonoscopy_parameters$prior_added_in_R
  ),
  Distribution = "beta",
  Source_ID = colonoscopy_parameters$source_id,
  stringsAsFactors = FALSE
)
sensitivity_inputs <- rbind(fit_input_table, colonoscopy_input_table)

metadata <- data.frame(
  Field = c(
    "Age range",
    "FIT threshold",
    "FIT selection",
    "Adherence definition",
    "Adherence distribution",
    "Adjusted population detection rate",
    "Modelled prevalence",
    "Specificity",
    "Number of simulations",
    "Base seed",
    "FIT-selection seed",
    "Model seed",
    "Colonoscopy seed",
    "External FIT seed",
    "Adherence seed"
  ),
  Value = c(
    "50-75 years",
    ">=100 ng Hb/mL",
    "One observed FIT selected at random per person",
    "Colonoscopy in the prespecified window among people with a positive selected FIT",
    "Beta(Colonoscopy_window + 0.5, FIT_positive - Colonoscopy_window + 0.5)",
    "d_j / (SeFIT_j * Adh)",
    "d_j / (SeFIT_j * Adh * SeCOL_j)",
    "Not used in the model equations",
    as.character(n_sim),
    seed_bases,
    as.character(seed_sensitivity),
    as.character(seed_model),
    as.character(seed_colonoscopy),
    as.character(seed_external_fit),
    as.character(seed_adherence)
  ),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# 7. Internal checks and export
# -----------------------------------------------------------------------------

expected_rows <- nrow(analysis_files) * 2L * 2L * length(lesion_keys)
result_keys <- paste(
  model_estimates$Analysis,
  model_estimates$Outcome,
  model_estimates$Lesion_key,
  model_estimates$FIT_sensitivity_source,
  sep = "::"
)
if (nrow(model_estimates) != expected_rows ||
    anyDuplicated(result_keys) ||
    any(!is.finite(as.matrix(model_estimates[, c(
      "Mean", "Lower_95", "Upper_95"
    )]))) ||
    any(model_estimates$Lower_95 > model_estimates$Mean) ||
    any(model_estimates$Mean > model_estimates$Upper_95)) {
  stop("Model estimates failed the final consistency checks.", call. = FALSE)
}

if (any(vapply(simulation_results, function(result) {
  any(vapply(c(
    result$adjusted_detection,
    result$modelled_prevalence
  ), function(x) any(x < 0), logical(1)))
}, logical(1)))) {
  stop("Negative simulated rates were found.", call. = FALSE)
}

write_tsv <- function(x, filename) {
  write.table(
    x,
    file.path(output_directory, filename),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )
}

write_tsv(model_estimates, "model_estimates.tsv")
write_tsv(
  model_estimates[model_estimates$Sex == "Both sexes", , drop = FALSE],
  "model_estimates_both_sexes.tsv"
)
write_tsv(colonoscopy_detection, "colonoscopy_detection_rates.tsv")
write_tsv(adherence_draw_summary, "adherence_parameters.tsv")
write_tsv(sensitivity_inputs, "sensitivity_inputs.tsv")
write_tsv(metadata, "metadata.tsv")

saveRDS(
  list(
    estimates = model_estimates,
    colonoscopy_detection = colonoscopy_detection,
    adherence = adherence_draw_summary,
    sensitivity_inputs = sensitivity_inputs,
    metadata = metadata,
    draws = simulation_results,
    colonoscopy_sensitivity_draws = colonoscopy_draws,
    external_fit_sensitivity_draws = external_fit_draws
  ),
  file.path(output_directory, "prenec_publication_results.rds"),
  compress = "xz"
)

message(
  "Results written to: ",
  normalizePath(output_directory, winslash = "/", mustWork = TRUE)
)

print(model_estimates[
  model_estimates$Sex == "Both sexes",
  c(
    "Base", "Outcome", "Lesion", "FIT_sensitivity_source",
    "Mean_percent", "Lower_95_percent", "Upper_95_percent"
  )
])

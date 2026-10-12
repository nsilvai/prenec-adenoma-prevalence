# -*- coding: UTF-8 -*-

# Stage 11: compare joint-bootstrap and current publication estimates ---------
#
# This supplementary stage creates a reproducible one-to-one crosswalk between
# the 96 estimates produced by the current publication analysis and the Full
# analysis rows from the joint participant-bootstrap analysis. It does not
# modify either source table.

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Configuration and input files
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

resolve_table <- function(location, filename) {
  if (tolower(tools::file_ext(location)) == "tsv") {
    return(location)
  }
  file.path(location, filename)
}

publication_output_location <- Sys.getenv(
  "PRENEC_PUBLICATION_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "publication")
)
joint_output_location <- Sys.getenv(
  "PRENEC_JOINT_BOOTSTRAP_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "joint_bootstrap")
)

publication_file <- resolve_table(
  publication_output_location,
  "model_estimates.tsv"
)
joint_file <- resolve_table(
  joint_output_location,
  "joint_bootstrap_run_summaries.tsv"
)
joint_output_directory <- if (
  tolower(tools::file_ext(joint_output_location)) == "tsv"
) {
  dirname(joint_output_location)
} else {
  joint_output_location
}
comparison_file <- file.path(
  joint_output_directory,
  "joint_vs_current_publication.tsv"
)

required_files <- c(publication_file, joint_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Required input files were not found:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}
dir.create(joint_output_directory, recursive = TRUE, showWarnings = FALSE)

read_tsv <- function(path) {
  read.delim(
    path,
    sep = "\t",
    header = TRUE,
    quote = "",
    comment.char = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

require_columns <- function(data, required, table_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      table_name,
      " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

publication <- read_tsv(publication_file)
joint_all <- read_tsv(joint_file)

publication_required <- c(
  "Analysis", "Base", "Sex", "Outcome", "Lesion_key", "Lesion",
  "FIT_sensitivity_source", "Mean", "Lower_95", "Upper_95",
  "Population", "Simulations", "Interval"
)
joint_required <- c(
  "Assignment", "Population", "Estimand", "Lesion_key", "Lesion",
  "FIT_sensitivity_scenario", "Uncertainty_run", "Plugin_estimate",
  "Simulation_mean", "Percentile_lower_95", "Percentile_upper_95",
  "Probability_above_1", "N_bootstrap"
)
require_columns(publication, publication_required, "Publication estimates")
require_columns(joint_all, joint_required, "Joint-bootstrap summaries")

# -----------------------------------------------------------------------------
# 2. Harmonise identifiers and validate the one-to-one crosswalk
# -----------------------------------------------------------------------------

if (nrow(publication) != 96L) {
  stop(
    "Publication estimates contain ", nrow(publication),
    " rows; exactly 96 were expected.",
    call. = FALSE
  )
}
if (anyNA(publication$Interval) ||
    any(publication$Interval != "95% HPD interval")) {
  stop(
    "Publication estimates must report 95% HPD intervals for this comparison.",
    call. = FALSE
  )
}

joint <- joint_all[
  !is.na(joint_all$Uncertainty_run) &
    joint_all$Uncertainty_run == "Full analysis",
  ,
  drop = FALSE
]
if (nrow(joint) != 96L) {
  stop(
    "Joint-bootstrap summaries contain ", nrow(joint),
    " Full analysis rows; exactly 96 were expected.",
    call. = FALSE
  )
}

estimand_map <- c(
  "Adjusted population detection rate" =
    "Lesion detection under universal colonoscopy",
  "Modelled prevalence" = "Prevalence"
)
fit_map <- c(
  "PRENEC-derived FIT sensitivity" = "PRENEC",
  "Literature-based FIT sensitivity" = "Literature"
)

unknown_outcomes <- setdiff(unique(publication$Outcome), names(estimand_map))
if (length(unknown_outcomes) > 0L) {
  stop(
    "Publication estimates contain unmapped outcomes: ",
    paste(unknown_outcomes, collapse = ", "),
    call. = FALSE
  )
}
unknown_fit_sources <- setdiff(
  unique(publication$FIT_sensitivity_source),
  names(fit_map)
)
if (length(unknown_fit_sources) > 0L) {
  stop(
    "Publication estimates contain unmapped FIT sensitivity sources: ",
    paste(unknown_fit_sources, collapse = ", "),
    call. = FALSE
  )
}

publication$Mapped_estimand <- unname(estimand_map[publication$Outcome])
publication$Mapped_fit_scenario <- unname(
  fit_map[publication$FIT_sensitivity_source]
)

make_key <- function(assignment, population, estimand, lesion, fit_scenario) {
  paste(
    assignment,
    population,
    estimand,
    lesion,
    fit_scenario,
    sep = "\r"
  )
}

publication_key <- make_key(
  publication$Base,
  publication$Sex,
  publication$Mapped_estimand,
  publication$Lesion_key,
  publication$Mapped_fit_scenario
)
joint_key <- make_key(
  joint$Assignment,
  joint$Population,
  joint$Estimand,
  joint$Lesion_key,
  joint$FIT_sensitivity_scenario
)

if (anyDuplicated(publication_key)) {
  stop(
    "Publication estimates do not have unique comparison keys.",
    call. = FALSE
  )
}
if (anyDuplicated(joint_key)) {
  stop(
    "Full joint-bootstrap estimates do not have unique comparison keys.",
    call. = FALSE
  )
}
if (!setequal(publication_key, joint_key)) {
  missing_from_joint <- setdiff(publication_key, joint_key)
  missing_from_publication <- setdiff(joint_key, publication_key)
  stop(
    "The comparison keys are not a one-to-one match. ",
    "Missing from joint bootstrap: ", length(missing_from_joint), "; ",
    "missing from publication estimates: ", length(missing_from_publication),
    ".",
    call. = FALSE
  )
}

joint_index <- match(publication_key, joint_key)
if (anyNA(joint_index) || anyDuplicated(joint_index) ||
    !identical(sort(joint_index), seq_len(nrow(joint)))) {
  stop("The comparison did not produce a strict one-to-one match.", call. = FALSE)
}
joint <- joint[joint_index, , drop = FALSE]

if (!all(publication$Lesion == joint$Lesion)) {
  stop("Lesion labels differ between matched rows.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 3. Numeric checks and comparison table
# -----------------------------------------------------------------------------

publication_numeric <- c("Mean", "Lower_95", "Upper_95")
joint_numeric <- c(
  "Plugin_estimate", "Simulation_mean", "Percentile_lower_95",
  "Percentile_upper_95", "Probability_above_1"
)
for (field in publication_numeric) {
  publication[[field]] <- suppressWarnings(as.numeric(publication[[field]]))
}
for (field in joint_numeric) {
  joint[[field]] <- suppressWarnings(as.numeric(joint[[field]]))
}

if (any(!is.finite(as.matrix(publication[, publication_numeric])))) {
  stop("Publication estimates contain non-finite numeric values.", call. = FALSE)
}
if (any(!is.finite(as.matrix(joint[, joint_numeric])))) {
  stop("Joint-bootstrap summaries contain non-finite numeric values.", call. = FALSE)
}
if (any(publication$Lower_95 > publication$Mean) ||
    any(publication$Mean > publication$Upper_95)) {
  stop("Publication HPD intervals are not ordered around their means.", call. = FALSE)
}
if (any(joint$Percentile_lower_95 > joint$Percentile_upper_95)) {
  stop("Joint-bootstrap percentile intervals are not ordered.", call. = FALSE)
}

current_width <- publication$Upper_95 - publication$Lower_95
joint_width <- joint$Percentile_upper_95 - joint$Percentile_lower_95
if (any(!is.finite(current_width)) || any(current_width <= 0) ||
    any(!is.finite(joint_width)) || any(joint_width <= 0)) {
  stop("All interval widths must be finite and strictly positive.", call. = FALSE)
}

comparison <- data.frame(
  Assignment = publication$Base,
  Population = publication$Sex,
  Estimand = publication$Mapped_estimand,
  Lesion_key = publication$Lesion_key,
  Lesion = publication$Lesion,
  FIT_sensitivity_scenario = publication$Mapped_fit_scenario,
  Current_analysis = publication$Analysis,
  Current_outcome_label = publication$Outcome,
  Current_FIT_sensitivity_source = publication$FIT_sensitivity_source,
  Current_mean = publication$Mean,
  Current_HPD_lower_95 = publication$Lower_95,
  Current_HPD_upper_95 = publication$Upper_95,
  Joint_plugin_estimate = joint$Plugin_estimate,
  Joint_simulation_mean = joint$Simulation_mean,
  Joint_percentile_lower_95 = joint$Percentile_lower_95,
  Joint_percentile_upper_95 = joint$Percentile_upper_95,
  Joint_plugin_minus_current_mean = joint$Plugin_estimate - publication$Mean,
  Joint_simulation_mean_minus_current_mean =
    joint$Simulation_mean - publication$Mean,
  Joint_plugin_minus_current_mean_percentage_points =
    100 * (joint$Plugin_estimate - publication$Mean),
  Joint_simulation_mean_minus_current_mean_percentage_points =
    100 * (joint$Simulation_mean - publication$Mean),
  Current_interval_width = current_width,
  Joint_interval_width = joint_width,
  Current_interval_width_percentage_points = 100 * current_width,
  Joint_interval_width_percentage_points = 100 * joint_width,
  Joint_to_current_interval_width_ratio = joint_width / current_width,
  Joint_probability_above_1 = joint$Probability_above_1,
  Current_population = suppressWarnings(as.numeric(publication$Population)),
  Current_simulations = suppressWarnings(as.integer(publication$Simulations)),
  Current_interval_type = publication$Interval,
  Joint_uncertainty_run = joint$Uncertainty_run,
  Joint_bootstrap_replicates = suppressWarnings(as.integer(joint$N_bootstrap)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (nrow(comparison) != 96L || anyDuplicated(make_key(
  comparison$Assignment,
  comparison$Population,
  comparison$Estimand,
  comparison$Lesion_key,
  comparison$FIT_sensitivity_scenario
))) {
  stop("The completed comparison table failed its final key checks.", call. = FALSE)
}
if (any(!is.finite(comparison$Joint_to_current_interval_width_ratio)) ||
    any(comparison$Joint_to_current_interval_width_ratio <= 0)) {
  stop("Interval-width ratios must be finite and strictly positive.", call. = FALSE)
}

assignment_order <- c("A", "B")
population_order <- c("Both sexes", "Men", "Women")
estimand_order <- c(
  "Lesion detection under universal colonoscopy",
  "Prevalence"
)
fit_order <- c("PRENEC", "Literature")
lesion_order <- c("low_risk", "high_risk", "cancer", "adenoma_total")
row_order <- order(
  match(comparison$Assignment, assignment_order),
  match(comparison$Population, population_order),
  match(comparison$Estimand, estimand_order),
  match(comparison$FIT_sensitivity_scenario, fit_order),
  match(comparison$Lesion_key, lesion_order)
)
comparison <- comparison[row_order, , drop = FALSE]
rownames(comparison) <- NULL

write.table(
  comparison,
  file = comparison_file,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  na = "NA",
  fileEncoding = "UTF-8"
)

message(
  "Joint-versus-current comparison written: ",
  normalizePath(comparison_file, winslash = "/", mustWork = TRUE),
  " (", nrow(comparison), " matched rows)"
)

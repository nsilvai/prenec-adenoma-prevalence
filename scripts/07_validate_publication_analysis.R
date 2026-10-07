# -*- coding: UTF-8 -*-

# Stage 07: reproducibility checks for the manuscript analysis ----------------
# Inputs: the stage-06 result object. Outputs: a validation table. An optional
# protected historical RDS can be supplied through PRENEC_REFERENCE_RESULT.

script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- sub(
  "^--file=", "",
  grep("^--file=", script_arguments, value = TRUE)[[1]]
)
project_directory <- Sys.getenv(
  "PRENEC_PROJECT_ROOT",
  unset = normalizePath(
    file.path(dirname(script_file), ".."),
    winslash = "/",
    mustWork = TRUE
  )
)
output_directory <- Sys.getenv(
  "PRENEC_PUBLICATION_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "publication")
)
result_file <- file.path(output_directory, "prenec_publication_results.rds")
reference_file <- Sys.getenv(
  "PRENEC_REFERENCE_RESULT",
  unset = file.path(
    project_directory,
    "data",
    "private",
    "prueba_incertidumbre_adherencia.rds"
  )
)

if (!file.exists(result_file)) {
  stop("Run analisis_publicacion_lancet_adherencia_aleatoria.R first.", call. = FALSE)
}

result <- readRDS(result_file)
estimates <- result$estimates
checks <- list()
add_check <- function(name, value, details) {
  checks[[length(checks) + 1L]] <<- data.frame(
    Check = name,
    Value = as.character(value),
    Status = ifelse(isTRUE(value), "OK", "ERROR"),
    Details = details,
    stringsAsFactors = FALSE
  )
}

keys <- paste(
  estimates$Analysis,
  estimates$Outcome,
  estimates$Lesion_key,
  estimates$FIT_sensitivity_source,
  sep = "::"
)
add_check(
  "Expected result rows",
  nrow(estimates) == 72L,
  paste("Observed", nrow(estimates), "rows; expected 72")
)
add_check(
  "Unique result keys",
  !anyDuplicated(keys),
  "Analysis, outcome, lesion, and FIT source uniquely identify each row"
)
add_check(
  "Reported lesions",
  setequal(
    unique(estimates$Lesion_key),
    c("low_risk", "high_risk", "cancer")
  ),
  paste(sort(unique(estimates$Lesion_key)), collapse = ", ")
)
add_check(
  "Reported outcomes",
  setequal(
    unique(estimates$Outcome),
    c("Adjusted population detection rate", "Modelled prevalence")
  ),
  paste(sort(unique(estimates$Outcome)), collapse = ", ")
)
add_check(
  "Finite estimates",
  all(is.finite(as.matrix(estimates[, c("Mean", "Lower_95", "Upper_95")]))),
  "Mean and 95% interval limits"
)
add_check(
  "Ordered intervals",
  all(estimates$Lower_95 <= estimates$Mean &
        estimates$Mean <= estimates$Upper_95),
  "Lower <= mean <= upper"
)

for (analysis_name in names(result$draws)) {
  analysis <- result$draws[[analysis_name]]
  add_check(
    paste("Finite draws", analysis_name),
    all(vapply(c(
      analysis$adjusted_detection,
      analysis$modelled_prevalence
    ), function(x) all(is.finite(x)), logical(1))),
    "Both FIT-sensitivity scenarios and both outcomes"
  )
}

max_draw_difference <- NA_real_
if (file.exists(reference_file)) {
  reference <- readRDS(reference_file)$scenario_results
  analysis_map <- c(
    A_Total = "A_Total",
    A_Men = "A_Hombres",
    A_Women = "A_Mujeres",
    B_Total = "B_Total",
    B_Men = "B_Hombres",
    B_Women = "B_Mujeres"
  )
  category_map <- c(
    low_risk = "Low risk adenomas",
    high_risk = "High risk adenomas",
    cancer = "Cancer"
  )
  differences <- numeric(0)

  for (new_name in names(analysis_map)) {
    old_name <- unname(analysis_map[[new_name]])
    new_analysis <- result$draws[[new_name]]

    for (key in names(category_map)) {
      category <- unname(category_map[[key]])

      old_local <- reference$PRENEC$sampled[[old_name]]
      differences <- c(
        differences,
        new_analysis$adjusted_detection$PRENEC[, key] -
          old_local$draws_tasa_deteccion_ajustada[, category],
        new_analysis$modelled_prevalence$PRENEC[, key] -
          old_local$draws[, category]
      )

      external_scenario <- if (key == "low_risk") {
        "meta_naa_oc100"
      } else {
        "lin_2021_oc_sensor"
      }
      old_external <- reference[[external_scenario]]$sampled[[old_name]]
      differences <- c(
        differences,
        new_analysis$adjusted_detection$Literature[, key] -
          old_external$draws_tasa_deteccion_ajustada[, category],
        new_analysis$modelled_prevalence$Literature[, key] -
          old_external$draws[, category]
      )
    }
  }
  max_draw_difference <- max(abs(differences))
  add_check(
    "Draw-level numerical equivalence",
    max_draw_difference < 1e-12,
    paste0("Maximum absolute difference: ", format(
      max_draw_difference,
      scientific = TRUE,
      digits = 16
    ))
  )
} else {
  checks[[length(checks) + 1L]] <- data.frame(
    Check = "Draw-level numerical equivalence",
    Value = "Not evaluated",
    Status = "NOT RUN",
    Details = "Reference result object was not available",
    stringsAsFactors = FALSE
  )
}

checks <- do.call(rbind, checks)
rownames(checks) <- NULL
write.table(
  checks,
  file.path(output_directory, "validation_checks.tsv"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

print(checks)
if (any(checks$Status == "ERROR")) {
  stop("At least one reproducibility check failed.", call. = FALSE)
}

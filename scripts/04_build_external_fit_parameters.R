# -*- coding: UTF-8 -*-

# Stage 04: literature FIT-sensitivity evidence -------------------------------
# Formal meta-analysis of per-patient FIT sensitivity for non-advanced adenoma.
# The four 2x2 counts below were supplied by the user. Before publication,
# confirm the analysis unit, OC-Sensor threshold, and full citations against
# each original article.

if (!requireNamespace("meta", quietly = TRUE)) {
  stop("The 'meta' package is required.", call. = FALSE)
}

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
output_directory <- Sys.getenv(
  "PRENEC_FIT_NONADVANCED_META_DIR",
  unset = file.path(
    project_directory,
    "results",
    "evidence",
    "fit_nonadvanced_meta_analysis"
  )
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

studies <- data.frame(
  Study = c(
    "Park 2010",
    "Redwood 2016",
    "Chang 2017",
    "Imperiale et al. 2014"
  ),
  Cutoff_ng_mL = rep(100, 4L),
  TP = c(20L, 24L, 87L, 220L),
  FN = c(199L, 211L, 1167L, 2693L),
  stringsAsFactors = FALSE
)

if (nrow(studies) != 4L) {
  stop("Exactly four supplied studies were expected.", call. = FALSE)
}
if (any(studies$Cutoff_ng_mL != 100) ||
    any(studies$TP < 0L) || any(studies$FN < 0L) ||
    any(studies$TP != as.integer(studies$TP)) ||
    any(studies$FN != as.integer(studies$FN))) {
  stop("Study inputs failed validation.", call. = FALSE)
}

wilson_interval <- function(events, total, conf_level = 0.95) {
  z <- qnorm(1 - (1 - conf_level) / 2)
  p <- events / total
  denominator <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / denominator
  half_width <- z * sqrt((p * (1 - p) + z^2 / (4 * total)) / total) /
    denominator
  cbind(
    lower = pmax(0, centre - half_width),
    upper = pmin(1, centre + half_width)
  )
}

studies$Total_cases <- studies$TP + studies$FN
studies$Sensitivity <- studies$TP / studies$Total_cases
study_wilson <- wilson_interval(studies$TP, studies$Total_cases)
studies$Wilson_95_lower <- study_wilson[, "lower"]
studies$Wilson_95_upper <- study_wilson[, "upper"]

# Random-effects binomial-logit GLMM. Hartung-Knapp is used for the primary
# confidence interval because only four studies were supplied. The conventional
# normal interval is also retained for reproducibility and comparison.
fit <- meta::metaprop(
  event = TP,
  n = Total_cases,
  studlab = Study,
  data = studies,
  sm = "PLOGIT",
  method = "GLMM",
  common = TRUE,
  random = TRUE,
  method.random.ci = "HK",
  prediction = TRUE
)

logit_mean <- as.numeric(fit$TE.random)
logit_se_model <- as.numeric(fit$seTE.random)
estimate <- plogis(logit_mean)
ci_normal <- plogis(logit_mean + c(-1, 1) * qnorm(0.975) * logit_se_model)
ci_hk <- c(
  lower = plogis(as.numeric(fit$lower.random)),
  upper = plogis(as.numeric(fit$upper.random))
)
prediction_hk <- c(
  lower = plogis(as.numeric(fit$lower.predict)),
  upper = plogis(as.numeric(fit$upper.predict))
)

# The prevalence model draws literature sensitivity from a logit-normal
# distribution. This SD is calibrated so that its central 95% interval equals
# the Hartung-Knapp interval; the unadjusted GLMM SE is reported separately.
logit_se_hk_calibrated <-
  (qlogis(ci_hk[["upper"]]) - qlogis(ci_hk[["lower"]])) /
  (2 * qnorm(0.975))

i2_raw <- as.numeric(fit$I2)
i2_lower_raw <- as.numeric(fit$lower.I2)
i2_upper_raw <- as.numeric(fit$upper.I2)
as_proportion <- function(x) ifelse(is.finite(x) & x > 1, x / 100, x)

meta_summary <- data.frame(
  Outcome = "Non-advanced adenoma",
  Lesion_key = "low_risk",
  FIT_family = "OC-Sensor",
  Cutoff_ng_mL = 100,
  Analysis_unit = "Per patient (assumption from supplied TP/FN; verify)",
  Method = "Random-effects binomial-logit GLMM; Hartung-Knapp CI",
  Studies_k = nrow(studies),
  Total_TP = sum(studies$TP),
  Total_FN = sum(studies$FN),
  Total_cases = sum(studies$Total_cases),
  Estimate = estimate,
  CI95_HK_lower = ci_hk[["lower"]],
  CI95_HK_upper = ci_hk[["upper"]],
  CI95_normal_lower = ci_normal[[1]],
  CI95_normal_upper = ci_normal[[2]],
  Prediction95_lower = prediction_hk[["lower"]],
  Prediction95_upper = prediction_hk[["upper"]],
  Logit_mean = logit_mean,
  Logit_SE_model = logit_se_model,
  Logit_SE_HK_calibrated = logit_se_hk_calibrated,
  Tau2 = as.numeric(fit$tau2),
  Q_Wald = unname(as.numeric(fit$Q[["Wald"]])),
  Q_Wald_p_value = unname(as.numeric(fit$pval.Q[[1]])),
  Q_LRT = unname(as.numeric(fit$Q[["LRT"]])),
  Q_LRT_p_value = unname(as.numeric(fit$pval.Q[[2]])),
  Q_df = nrow(studies) - 1L,
  I2 = as_proportion(i2_raw),
  I2_95_lower = as_proportion(i2_lower_raw),
  I2_95_upper = as_proportion(i2_upper_raw),
  stringsAsFactors = FALSE
)

pooled_wilson <- wilson_interval(sum(studies$TP), sum(studies$Total_cases))
secondary_checks <- data.frame(
  Check = c(
    "Crude patient-weighted sensitivity",
    "Crude Wilson 95% lower",
    "Crude Wilson 95% upper",
    "Unweighted mean of study sensitivities"
  ),
  Value = c(
    sum(studies$TP) / sum(studies$Total_cases),
    pooled_wilson[1, "lower"],
    pooled_wilson[1, "upper"],
    mean(studies$Sensitivity)
  ),
  stringsAsFactors = FALSE
)

write_tsv <- function(x, filename) {
  path <- file.path(output_directory, filename)
  write.table(
    x,
    path,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = "",
    fileEncoding = "UTF-8"
  )
  path
}

paths <- c(
  studies = write_tsv(studies, "estudios_sensibilidad_fit_non_advanced.tsv"),
  summary = write_tsv(meta_summary, "resumen_meta_sensibilidad_fit_non_advanced.tsv"),
  checks = write_tsv(secondary_checks, "controles_meta_sensibilidad_fit_non_advanced.tsv")
)
model_path <- file.path(output_directory, "modelo_meta_sensibilidad_fit_non_advanced.rds")
saveRDS(
  list(
    studies = studies,
    fit = fit,
    summary = meta_summary,
    secondary_checks = secondary_checks
  ),
  model_path,
  version = 3
)

message("Formal FIT sensitivity meta-analysis completed: ", output_directory)
print(studies[, c(
  "Study", "TP", "FN", "Total_cases", "Sensitivity",
  "Wilson_95_lower", "Wilson_95_upper"
)])
print(meta_summary[, c(
  "Studies_k", "Total_TP", "Total_FN", "Total_cases", "Estimate",
  "CI95_HK_lower", "CI95_HK_upper", "CI95_normal_lower",
  "CI95_normal_upper", "Tau2", "I2", "Q_Wald", "Q_Wald_p_value",
  "Q_LRT", "Q_LRT_p_value",
  "Logit_mean", "Logit_SE_model", "Logit_SE_HK_calibrated"
)])

invisible(list(
  studies = studies,
  fit = fit,
  summary = meta_summary,
  secondary_checks = secondary_checks,
  paths = c(paths, model = model_path)
))

options(stringsAsFactors = FALSE)

# Stage 12: joint-bootstrap publication figures -------------------------------
# This stage creates the current manuscript figures from the full
# joint-bootstrap analysis. It does not overwrite the separate Stage 08 figure.

configured_project_directory <- Sys.getenv("PRENEC_PROJECT_ROOT", unset = "")
if (nzchar(configured_project_directory)) {
  project_directory <- normalizePath(
    configured_project_directory,
    winslash = "/",
    mustWork = TRUE
  )
} else {
  script_arguments <- commandArgs(trailingOnly = FALSE)
  script_matches <- grep("^--file=", script_arguments, value = TRUE)
  if (length(script_matches) == 0L) {
    stop(
      "Set PRENEC_PROJECT_ROOT when sourcing this script interactively.",
      call. = FALSE
    )
  }
  script_file <- sub("^--file=", "", script_matches[[1]])
  project_directory <- normalizePath(
    file.path(dirname(script_file), ".."),
    winslash = "/",
    mustWork = TRUE
  )
}

joint_output_directory <- Sys.getenv(
  "PRENEC_JOINT_BOOTSTRAP_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "joint_bootstrap")
)
input_file <- file.path(
  joint_output_directory,
  "joint_bootstrap_run_summaries.tsv"
)
output_directory <- file.path(joint_output_directory, "figures")

required_packages <- c("ggplot2", "patchwork", "ragg")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(ggplot2)
library(patchwork)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

results <- read.delim(input_file, check.names = FALSE)
required_columns <- c(
  "Assignment", "Population", "Estimand", "Lesion_key", "Lesion",
  "FIT_sensitivity_scenario", "Uncertainty_run", "Plugin_estimate",
  "Percentile_lower_95", "Percentile_upper_95"
)
missing_columns <- setdiff(required_columns, names(results))
if (length(missing_columns) > 0L) {
  stop(
    "Input is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

lesion_levels <- c(
  "Low-risk adenoma",
  "High-risk adenoma",
  "Any adenoma",
  "Colorectal cancer"
)
lesion_key_levels <- c("low_risk", "high_risk", "adenoma_total", "cancer")
estimand_levels <- c(
  "Lesion detection under universal colonoscopy",
  "Prevalence"
)
population_levels <- c("Both sexes", "Men", "Women")
scenario_levels <- c("PRENEC", "Literature")
scenario_labels <- c(
  "PRENEC" = "PRENEC-derived FIT\nsensitivity",
  "Literature" = "Literature-based FIT\nsensitivity"
)
assignment_levels <- c("A", "B")

full_results <- results[
  results$Uncertainty_run == "Full analysis",
  required_columns,
  drop = FALSE
]

if (nrow(full_results) != 96L) {
  stop(
    "Expected 96 full-analysis rows but found ", nrow(full_results), ".",
    call. = FALSE
  )
}
if (any(table(
  factor(full_results$Assignment, levels = assignment_levels),
  factor(full_results$Population, levels = population_levels),
  factor(full_results$Estimand, levels = estimand_levels),
  factor(full_results$Lesion_key, levels = lesion_key_levels),
  factor(full_results$FIT_sensitivity_scenario, levels = scenario_levels)
) != 1L)) {
  stop(
    paste(
      "Each assignment, population, estimand, lesion, and FIT-sensitivity",
      "scenario must occur exactly once in the full analysis."
    ),
    call. = FALSE
  )
}

plot_data <- data.frame(
  Assignment = full_results$Assignment,
  Population = full_results$Population,
  Estimand = full_results$Estimand,
  Lesion_key = full_results$Lesion_key,
  Lesion = full_results$Lesion,
  Scenario = full_results$FIT_sensitivity_scenario,
  Estimate = 100 * full_results$Plugin_estimate,
  Lower = 100 * full_results$Percentile_lower_95,
  Upper = 100 * full_results$Percentile_upper_95,
  stringsAsFactors = FALSE
)

if (any(!is.finite(as.matrix(plot_data[c("Estimate", "Lower", "Upper")])))) {
  stop("All estimates and interval limits must be finite.", call. = FALSE)
}
if (any(plot_data$Lower < 0) || any(plot_data$Upper < plot_data$Lower)) {
  stop("All intervals must be ordered and non-negative.", call. = FALSE)
}
if (any(plot_data$Estimate < plot_data$Lower) ||
    any(plot_data$Estimate > plot_data$Upper)) {
  stop("Each plug-in estimate must lie within its 95% interval.", call. = FALSE)
}

plot_data$Assignment <- factor(
  plot_data$Assignment,
  levels = assignment_levels
)
plot_data$Population <- factor(
  plot_data$Population,
  levels = population_levels
)
plot_data$Estimand <- factor(
  plot_data$Estimand,
  levels = estimand_levels
)
plot_data$Lesion <- factor(plot_data$Lesion, levels = lesion_levels)
plot_data$Lesion_key <- factor(
  plot_data$Lesion_key,
  levels = lesion_key_levels
)
plot_data$Scenario <- factor(plot_data$Scenario, levels = scenario_levels)

main_data <- plot_data[plot_data$Population == "Both sexes", , drop = FALSE]
supplementary_data <- plot_data[
  plot_data$Population %in% c("Men", "Women"),
  ,
  drop = FALSE
]
if (nrow(main_data) != 32L) {
  stop("Expected 32 rows for the main figure.", call. = FALSE)
}
if (nrow(supplementary_data) != 64L) {
  stop("Expected 64 rows for the supplementary figure.", call. = FALSE)
}

# Scenarios form the two main rows; assignments A and B are offset within each
# scenario and distinguished by both interval linetype and point fill.
scenario_y <- c("PRENEC" = 2, "Literature" = 1)
assignment_offset <- c("A" = 0.15, "B" = -0.15)
plot_data$Y <-
  unname(scenario_y[as.character(plot_data$Scenario)]) +
  unname(assignment_offset[as.character(plot_data$Assignment)])
plot_data$LabelY <- plot_data$Y + ifelse(
  plot_data$Assignment == "A",
  0.12,
  -0.12
)
plot_data$PointLabel <- ifelse(
  plot_data$Lesion == "Colorectal cancer",
  sprintf("%.3f%%", plot_data$Estimate),
  sprintf("%.1f%%", plot_data$Estimate)
)
main_data <- plot_data[plot_data$Population == "Both sexes", , drop = FALSE]
supplementary_data <- plot_data[
  plot_data$Population %in% c("Men", "Women"),
  ,
  drop = FALSE
]

main_limits <- c(
  "Low-risk adenoma" = 50,
  "High-risk adenoma" = 20,
  "Any adenoma" = 50,
  "Colorectal cancer" = 0.8
)
supplementary_limits <- c(
  "Low-risk adenoma" = 70,
  "High-risk adenoma" = 30,
  "Any adenoma" = 70,
  "Colorectal cancer" = 1.2
)

validate_axis_limits <- function(data, limits, figure_name) {
  upper_by_lesion <- tapply(data$Upper, data$Lesion, max)
  limits_aligned <- limits[names(upper_by_lesion)]
  if (any(!is.finite(limits_aligned)) || any(upper_by_lesion > limits_aligned)) {
    offending <- names(upper_by_lesion)[upper_by_lesion > limits_aligned]
    stop(
      figure_name,
      " axis limit is too small for: ",
      paste(offending, collapse = ", "),
      call. = FALSE
    )
  }
}
validate_axis_limits(main_data, main_limits, "Main figure")
validate_axis_limits(
  supplementary_data,
  supplementary_limits,
  "Supplementary figure"
)

axis_limit_data <- function(limits) {
  data.frame(
    Lesion = factor(rep(names(limits), each = 2L), levels = lesion_levels),
    X = as.vector(rbind(rep(0, length(limits)), as.numeric(limits))),
    Y = rep(1.5, 2L * length(limits))
  )
}

panel_breaks <- function(limits) {
  upper <- max(limits, na.rm = TRUE)
  if (upper <= 1.3) {
    pretty(c(0, upper), n = 5)
  } else if (upper <= 35) {
    pretty(c(0, upper), n = 5)
  } else {
    pretty(c(0, upper), n = 6)
  }
}

panel_labels <- function(x) {
  if (max(x, na.rm = TRUE) <= 1.3) {
    sprintf("%.1f", x)
  } else {
    sprintf("%.0f", x)
  }
}

create_panel <- function(
    data,
    population_name,
    estimand_name,
    panel_title,
    x_title,
    limits,
    show_strips = TRUE,
    show_legend = FALSE) {
  panel_data <- data[
    data$Population == population_name & data$Estimand == estimand_name,
    ,
    drop = FALSE
  ]
  if (nrow(panel_data) != 16L) {
    stop("Each plot panel must contain exactly 16 rows.", call. = FALSE)
  }

  panel_plot <- ggplot(panel_data) +
    geom_blank(
      data = axis_limit_data(limits),
      aes(x = X, y = Y),
      inherit.aes = FALSE
    ) +
    geom_hline(
      yintercept = 1.5,
      colour = "#D9D9D9",
      linewidth = 0.35
    ) +
    geom_segment(
      aes(
        x = Lower,
        xend = Upper,
        y = Y,
        yend = Y,
        linetype = Assignment
      ),
      colour = "#222222",
      linewidth = 0.58
    ) +
    geom_segment(
      aes(
        x = Lower,
        xend = Lower,
        y = Y - 0.045,
        yend = Y + 0.045,
        linetype = Assignment
      ),
      colour = "#222222",
      linewidth = 0.46
    ) +
    geom_segment(
      aes(
        x = Upper,
        xend = Upper,
        y = Y - 0.045,
        yend = Y + 0.045,
        linetype = Assignment
      ),
      colour = "#222222",
      linewidth = 0.46
    ) +
    geom_point(
      aes(x = Estimate, y = Y, shape = Assignment, fill = Assignment),
      colour = "#111111",
      size = 2.25,
      stroke = 0.68
    ) +
    geom_text(
      aes(x = Estimate, y = LabelY, label = PointLabel),
      family = "Arial",
      size = 2.25,
      colour = "#111111"
    ) +
    facet_grid(. ~ Lesion, scales = "free_x", space = "fixed") +
    scale_y_continuous(
      breaks = c(1, 2),
      labels = unname(scenario_labels[c("Literature", "PRENEC")]),
      limits = c(0.55, 2.45),
      expand = expansion(mult = 0)
    ) +
    scale_x_continuous(
      breaks = panel_breaks,
      labels = panel_labels,
      expand = expansion(mult = c(0.02, 0.035))
    ) +
    scale_shape_manual(
      name = "Analysis base",
      values = c("A" = 21, "B" = 21),
      labels = c("Base A", "Base B")
    ) +
    scale_fill_manual(
      name = "Analysis base",
      values = c("A" = "#111111", "B" = "white"),
      labels = c("Base A", "Base B")
    ) +
    scale_linetype_manual(
      name = "Analysis base",
      values = c("A" = "solid", "B" = "longdash"),
      labels = c("Base A", "Base B")
    ) +
    labs(title = panel_title, x = x_title, y = NULL) +
    guides(
      fill = "none",
      linetype = "none",
      shape = guide_legend(
        title.position = "left",
        override.aes = list(fill = c("#111111", "white"))
      )
    ) +
    theme_classic(base_family = "Arial", base_size = 8) +
    theme(
      plot.title = element_text(
        size = 9.4,
        face = "bold",
        hjust = 0,
        margin = margin(b = 4)
      ),
      strip.background = element_blank(),
      strip.text = element_text(
        size = 8.2,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 3)
      ),
      panel.spacing.x = grid::unit(6, "mm"),
      axis.text.x = element_text(size = 7.1, colour = "#111111"),
      axis.text.y = element_text(
        size = 7.3,
        colour = "#111111",
        hjust = 1,
        margin = margin(r = 4)
      ),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_text(size = 7.8, margin = margin(t = 4)),
      legend.position = if (show_legend) "bottom" else "none",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.title = element_text(size = 7.7, face = "bold"),
      legend.text = element_text(size = 7.7),
      legend.key.width = grid::unit(5, "mm"),
      legend.spacing.x = grid::unit(1.5, "mm"),
      plot.margin = margin(3, 5, 3, 5)
    )

  if (!show_strips) {
    panel_plot <- panel_plot + theme(strip.text.x = element_blank())
  }
  panel_plot
}

main_detection_panel <- create_panel(
  main_data,
  "Both sexes",
  "Lesion detection under universal colonoscopy",
  "(A) Lesion detection under universal colonoscopy among both sexes",
  "Lesion detection under universal colonoscopy (%)",
  main_limits,
  show_strips = TRUE,
  show_legend = FALSE
)
main_prevalence_panel <- create_panel(
  main_data,
  "Both sexes",
  "Prevalence",
  "(B) Prevalence among both sexes",
  "Prevalence (%)",
  main_limits,
  show_strips = FALSE,
  show_legend = TRUE
)
main_figure <-
  (main_detection_panel / plot_spacer() / main_prevalence_panel) +
  plot_layout(heights = c(1, 0.13, 1))

supplementary_panels <- list(
  create_panel(
    supplementary_data,
    "Men",
    "Lesion detection under universal colonoscopy",
    "(A) Lesion detection under universal colonoscopy among men",
    "Lesion detection under universal colonoscopy (%)",
    supplementary_limits,
    show_strips = TRUE,
    show_legend = FALSE
  ),
  create_panel(
    supplementary_data,
    "Men",
    "Prevalence",
    "(B) Prevalence among men",
    "Prevalence (%)",
    supplementary_limits,
    show_strips = FALSE,
    show_legend = FALSE
  ),
  create_panel(
    supplementary_data,
    "Women",
    "Lesion detection under universal colonoscopy",
    "(C) Lesion detection under universal colonoscopy among women",
    "Lesion detection under universal colonoscopy (%)",
    supplementary_limits,
    show_strips = FALSE,
    show_legend = FALSE
  ),
  create_panel(
    supplementary_data,
    "Women",
    "Prevalence",
    "(D) Prevalence among women",
    "Prevalence (%)",
    supplementary_limits,
    show_strips = FALSE,
    show_legend = TRUE
  )
)
supplementary_figure <-
  supplementary_panels[[1]] /
  supplementary_panels[[2]] /
  supplementary_panels[[3]] /
  supplementary_panels[[4]] +
  plot_layout(heights = c(1, 1, 1, 1))

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

save_figure <- function(plot, stem, width_mm, height_mm) {
  ggsave(
    filename = file.path(output_directory, paste0(stem, ".pdf")),
    plot = plot,
    device = grDevices::cairo_pdf,
    width = width_mm,
    height = height_mm,
    units = "mm"
  )
  ggsave(
    filename = file.path(output_directory, paste0(stem, ".png")),
    plot = plot,
    device = ragg::agg_png,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = 600
  )
  ggsave(
    filename = file.path(output_directory, paste0(stem, ".tiff")),
    plot = plot,
    device = ragg::agg_tiff,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = 600,
    compression = "lzw"
  )
  ggsave(
    filename = file.path(output_directory, paste0(stem, ".svg")),
    plot = plot,
    device = grDevices::svg,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    units = "in"
  )
}

main_stem <- "figure_joint_bootstrap_both_sexes"
supplementary_stem <- "figure_joint_bootstrap_by_sex_supplementary"
save_figure(main_figure, main_stem, width_mm = 250, height_mm = 145)
save_figure(
  supplementary_figure,
  supplementary_stem,
  width_mm = 250,
  height_mm = 265
)

write_plot_data <- function(data, filename) {
  data <- data[order(
    data$Population,
    data$Estimand,
    data$Lesion_key,
    data$Scenario,
    data$Assignment
  ), ]
  output <- data.frame(
    Population = as.character(data$Population),
    Estimand = as.character(data$Estimand),
    Lesion_key = as.character(data$Lesion_key),
    Lesion = as.character(data$Lesion),
    FIT_sensitivity_scenario = as.character(data$Scenario),
    Assignment = as.character(data$Assignment),
    Plugin_estimate_percent = data$Estimate,
    Percentile_lower_95_percent = data$Lower,
    Percentile_upper_95_percent = data$Upper,
    stringsAsFactors = FALSE
  )
  write.table(
    output,
    file = file.path(output_directory, filename),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
}

write_plot_data(main_data, "figure_joint_bootstrap_both_sexes_data.tsv")
write_plot_data(
  supplementary_data,
  "figure_joint_bootstrap_by_sex_supplementary_data.tsv"
)

caption_lines <- c(
  paste(
    "Main figure. Estimated lesion detection under universal colonoscopy and",
    "prevalence among PRENEC participants of both sexes. Points are plug-in",
    "estimates and horizontal lines are equal-tail 95% percentile intervals",
    "from the full joint uncertainty analysis. Participant records were",
    "jointly resampled within sex-age strata, while uncertainty in FIT and",
    "colonoscopy sensitivity was propagated separately. Rows denote the",
    "FIT-sensitivity scenario; filled points with solid intervals and open",
    "points with dashed intervals denote analysis bases A and B, respectively.",
    "Horizontal-axis scales differ by lesion category."
  ),
  "",
  paste(
    "Supplementary figure. Estimated lesion detection under universal",
    "colonoscopy and prevalence among men and women. Points, intervals,",
    "FIT-sensitivity rows, and analysis-base symbols follow the main figure.",
    "Within each lesion category, the same horizontal-axis scale is used for",
    "men and women and for both estimands; scales differ across lesion",
    "categories."
  )
)
writeLines(
  caption_lines,
  con = file.path(output_directory, "joint_bootstrap_figure_captions.txt"),
  useBytes = TRUE
)

expected_files <- file.path(
  output_directory,
  c(
    paste0(main_stem, c(".pdf", ".png", ".tiff", ".svg")),
    paste0(supplementary_stem, c(".pdf", ".png", ".tiff", ".svg")),
    "figure_joint_bootstrap_both_sexes_data.tsv",
    "figure_joint_bootstrap_by_sex_supplementary_data.tsv",
    "joint_bootstrap_figure_captions.txt"
  )
)
missing_outputs <- expected_files[!file.exists(expected_files)]
empty_outputs <- expected_files[
  file.exists(expected_files) & file.info(expected_files)$size <= 0
]
if (length(missing_outputs) > 0L || length(empty_outputs) > 0L) {
  stop("One or more figure outputs were not written correctly.", call. = FALSE)
}

message(
  "Joint-bootstrap figures written to: ",
  normalizePath(output_directory, winslash = "/", mustWork = TRUE)
)

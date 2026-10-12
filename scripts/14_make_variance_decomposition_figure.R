options(stringsAsFactors = FALSE)

# Stage 14: variance-source decomposition figure -----------------------------
#
# This figure reports assignment A, both sexes, for low-risk adenoma,
# high-risk adenoma, and colorectal cancer. Variance shares are taken from the
# exact additive log-scale decomposition produced by Stage 10.

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
  "joint_bootstrap_log_variance_shares.tsv"
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

variance_data <- read.delim(input_file, check.names = FALSE)
required_columns <- c(
  "Assignment", "Population", "Estimand", "Lesion_key", "Lesion",
  "FIT_sensitivity_scenario", "Data_variance_share",
  "FIT_variance_share", "Colonoscopy_variance_share"
)
missing_columns <- setdiff(required_columns, names(variance_data))
if (length(missing_columns) > 0L) {
  stop(
    "Input is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

lesion_key_levels <- c("low_risk", "high_risk", "cancer")
lesion_levels <- c(
  "Low-risk adenoma",
  "High-risk adenoma",
  "Colorectal cancer"
)
estimand_levels <- c(
  "Lesion detection under universal colonoscopy",
  "Prevalence"
)
scenario_levels <- c("PRENEC", "Literature")

figure_data <- variance_data[
  variance_data$Assignment == "A" &
    variance_data$Population == "Both sexes" &
    variance_data$Lesion_key %in% lesion_key_levels,
  required_columns,
  drop = FALSE
]

if (nrow(figure_data) != 12L) {
  stop("Expected 12 variance rows but found ", nrow(figure_data), ".", call. = FALSE)
}
cell_counts <- table(
  factor(figure_data$Estimand, levels = estimand_levels),
  factor(figure_data$Lesion_key, levels = lesion_key_levels),
  factor(figure_data$FIT_sensitivity_scenario, levels = scenario_levels)
)
if (any(cell_counts != 1L)) {
  stop("Each estimand, lesion, and FIT scenario must occur exactly once.", call. = FALSE)
}

shares <- as.matrix(figure_data[, c(
  "Data_variance_share",
  "FIT_variance_share",
  "Colonoscopy_variance_share"
)])
if (any(!is.finite(shares)) || any(shares < 0) ||
    max(abs(rowSums(shares) - 1)) > 1e-10) {
  stop("Variance shares must be finite, non-negative, and sum to one.", call. = FALSE)
}

long_data <- rbind(
  transform(
    figure_data,
    Source = "Screening data",
    Share = 100 * Data_variance_share
  ),
  transform(
    figure_data,
    Source = "FIT sensitivity",
    Share = 100 * FIT_variance_share
  ),
  transform(
    figure_data,
    Source = "Colonoscopy sensitivity",
    Share = 100 * Colonoscopy_variance_share
  )
)

long_data$Lesion <- factor(long_data$Lesion, levels = lesion_levels)
long_data$Estimand <- factor(long_data$Estimand, levels = estimand_levels)
long_data$FIT_sensitivity_scenario <- factor(
  long_data$FIT_sensitivity_scenario,
  levels = scenario_levels,
  labels = c("PRENEC-derived", "Literature-based")
)
long_data$Source <- factor(
  long_data$Source,
  levels = c(
    "Screening data",
    "FIT sensitivity",
    "Colonoscopy sensitivity"
  )
)
long_data$Segment_label <- ifelse(
  long_data$Share >= 6,
  sprintf("%.1f", long_data$Share),
  ""
)

source_colours <- c(
  "Screening data" = "#3568A8",
  "FIT sensitivity" = "#E69F00",
  "Colonoscopy sensitivity" = "#009E73"
)

create_panel <- function(data, estimand_name, panel_title, show_legend) {
  panel_data <- data[data$Estimand == estimand_name, , drop = FALSE]
  if (nrow(panel_data) != 18L) {
    stop("Each estimand panel must contain 18 long-format rows.", call. = FALSE)
  }

  ggplot(
    panel_data,
    aes(
      x = Share,
      y = FIT_sensitivity_scenario,
      fill = Source
    )
  ) +
    geom_col(
      width = 0.62,
      colour = "white",
      linewidth = 0.25,
      position = position_stack(reverse = TRUE)
    ) +
    geom_text(
      aes(label = Segment_label),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      family = "Arial",
      size = 2.65,
      colour = "white",
      fontface = "bold"
    ) +
    facet_grid(Lesion ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(
      breaks = seq(0, 100, by = 20),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(xlim = c(0, 100), expand = FALSE) +
    scale_fill_manual(values = source_colours, drop = FALSE) +
    labs(
      title = panel_title,
      x = "Share of total attributable log-scale variance",
      y = NULL,
      fill = "Uncertainty source"
    ) +
    theme_classic(base_family = "Arial", base_size = 8.5) +
    theme(
      plot.title = element_text(
        size = 9.7,
        face = "bold",
        hjust = 0,
        margin = margin(b = 5)
      ),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text.y = element_text(
        angle = 0,
        face = "bold",
        size = 8.4,
        hjust = 0,
        margin = margin(2, 5, 2, 5)
      ),
      panel.spacing.y = grid::unit(3.5, "mm"),
      axis.text.x = element_text(size = 7.6, colour = "#111111"),
      axis.text.y = element_text(size = 7.8, colour = "#111111"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_text(size = 8.1, margin = margin(t = 4)),
      legend.position = if (show_legend) "bottom" else "none",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.width = grid::unit(5.5, "mm"),
      legend.spacing.x = grid::unit(1.5, "mm"),
      plot.margin = margin(3, 5, 3, 5)
    )
}

detection_panel <- create_panel(
  long_data,
  estimand_levels[[1]],
  "(A) Lesion detection under universal colonoscopy",
  show_legend = FALSE
)
prevalence_panel <- create_panel(
  long_data,
  estimand_levels[[2]],
  "(B) Prevalence",
  show_legend = TRUE
)

variance_figure <- detection_panel / prevalence_panel +
  plot_layout(heights = c(1, 1.08))

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
stem <- "figure_variance_decomposition_sample_a"

ggsave(
  filename = file.path(output_directory, paste0(stem, ".pdf")),
  plot = variance_figure,
  device = grDevices::cairo_pdf,
  width = 178,
  height = 205,
  units = "mm"
)
ggsave(
  filename = file.path(output_directory, paste0(stem, ".png")),
  plot = variance_figure,
  device = ragg::agg_png,
  width = 178,
  height = 205,
  units = "mm",
  dpi = 600
)
ggsave(
  filename = file.path(output_directory, paste0(stem, ".tiff")),
  plot = variance_figure,
  device = ragg::agg_tiff,
  width = 178,
  height = 205,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)
ggsave(
  filename = file.path(output_directory, paste0(stem, ".svg")),
  plot = variance_figure,
  device = grDevices::svg,
  width = 178 / 25.4,
  height = 205 / 25.4,
  units = "in"
)

data_export <- long_data[order(
  long_data$Estimand,
  long_data$Lesion,
  long_data$FIT_sensitivity_scenario,
  long_data$Source
), c(
  "Assignment", "Population", "Estimand", "Lesion_key", "Lesion",
  "FIT_sensitivity_scenario", "Source", "Share"
)]
data_export$Lesion <- as.character(data_export$Lesion)
data_export$Estimand <- as.character(data_export$Estimand)
data_export$FIT_sensitivity_scenario <- as.character(
  data_export$FIT_sensitivity_scenario
)
data_export$Source <- as.character(data_export$Source)
write.table(
  data_export,
  file = file.path(output_directory, paste0(stem, "_data.tsv")),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

expected_files <- file.path(
  output_directory,
  c(
    paste0(stem, c(".pdf", ".png", ".tiff", ".svg")),
    paste0(stem, "_data.tsv")
  )
)
if (any(!file.exists(expected_files)) ||
    any(file.info(expected_files)$size <= 0)) {
  stop("One or more variance-figure outputs were not written.", call. = FALSE)
}

message(
  "Variance-decomposition figure written to: ",
  normalizePath(output_directory, winslash = "/", mustWork = TRUE)
)

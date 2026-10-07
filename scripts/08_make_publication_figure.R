options(stringsAsFactors = FALSE)

# Stage 08: publication figure -------------------------------------------------
# Publication figure for the analysis with sampled colonoscopy adherence.
# Estimates are presented for both sexes and for the three prespecified outcomes.

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
publication_output_directory <- Sys.getenv(
  "PRENEC_PUBLICATION_OUTPUT_DIR",
  unset = file.path(project_directory, "results", "publication")
)

required_packages <- c("ggplot2", "patchwork", "ragg", "svglite")
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

input_file <- file.path(
  publication_output_directory,
  "model_estimates_both_sexes.tsv"
)
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

results <- read.delim(input_file, check.names = FALSE)

lesion_levels <- c(
  "Low-risk adenoma",
  "High-risk adenoma",
  "Colorectal cancer"
)
outcome_levels <- c(
  "Adjusted population detection rate",
  "Modelled prevalence"
)
scenario_levels <- c(
  "PRENEC-derived FIT sensitivity",
  "Literature-based FIT sensitivity"
)
base_levels <- c("A", "B")

plot_data <- results[
  results$Sex == "Both sexes" &
    results$Lesion %in% lesion_levels &
    results$Outcome %in% outcome_levels &
    results$FIT_sensitivity_source %in% scenario_levels &
    results$Base %in% base_levels,
  c(
    "Outcome", "Lesion", "FIT_sensitivity_source", "Base",
    "Mean_percent", "Lower_95_percent", "Upper_95_percent"
  )
]

names(plot_data) <- c(
  "Outcome", "Lesion", "Scenario", "Base",
  "Estimate", "Lower", "Upper"
)

plot_data$Outcome <- factor(plot_data$Outcome, levels = outcome_levels)
plot_data$Lesion <- factor(plot_data$Lesion, levels = lesion_levels)
plot_data$Scenario <- factor(plot_data$Scenario, levels = scenario_levels)
plot_data$Base <- factor(plot_data$Base, levels = base_levels)

# Each cell of the figure must contain one estimate and one 95% interval.
expected_rows <-
  length(outcome_levels) *
  length(lesion_levels) *
  length(scenario_levels) *
  length(base_levels)

if (nrow(plot_data) != expected_rows) {
  stop(
    "Expected ", expected_rows, " rows but found ", nrow(plot_data), ".",
    call. = FALSE
  )
}
if (any(table(
  plot_data$Outcome,
  plot_data$Lesion,
  plot_data$Scenario,
  plot_data$Base
) != 1L)) {
  stop(
    paste(
      "Each outcome, lesion, FIT-sensitivity scenario, and analysis base",
      "must occur exactly once."
    ),
    call. = FALSE
  )
}
if (any(!is.finite(as.matrix(plot_data[c(
  "Estimate", "Lower", "Upper"
)])))) {
  stop("All estimates and interval limits must be finite.", call. = FALSE)
}
if (any(plot_data$Lower > plot_data$Estimate) ||
    any(plot_data$Estimate > plot_data$Upper)) {
  stop("Each estimate must lie within its 95% interval.", call. = FALSE)
}

# FIT-sensitivity scenarios form the two main rows. Bases A and B are offset
# within each scenario and identified once in the shared legend.
scenario_y <- c(
  "PRENEC-derived FIT sensitivity" = 2,
  "Literature-based FIT sensitivity" = 1
)
base_offset <- c("A" = 0.15, "B" = -0.15)

plot_data$Y <-
  unname(scenario_y[as.character(plot_data$Scenario)]) +
  unname(base_offset[as.character(plot_data$Base)])
plot_data$LabelY <- plot_data$Y + ifelse(
  plot_data$Base == "A",
  0.12,
  -0.12
)
plot_data$PointLabel <- ifelse(
  plot_data$Lesion == "Colorectal cancer",
  sprintf("%.3f%%", plot_data$Estimate),
  sprintf("%.1f%%", plot_data$Estimate)
)

# Fixed panel limits allow direct comparison between the two outcome rows.
axis_limits <- data.frame(
  Lesion = factor(rep(lesion_levels, each = 2L), levels = lesion_levels),
  X = c(0, 50, 0, 20, 0, 1),
  Y = 1.5
)

panel_breaks <- function(limits) {
  upper <- max(limits, na.rm = TRUE)
  if (upper <= 1.1) {
    seq(0, 1, by = 0.2)
  } else if (upper <= 25) {
    seq(0, 20, by = 5)
  } else {
    seq(0, 50, by = 10)
  }
}

panel_labels <- function(x) {
  if (max(x, na.rm = TRUE) <= 1.1) {
    sprintf("%.1f", x)
  } else {
    sprintf("%.0f", x)
  }
}

create_outcome_panel <- function(outcome_name, panel_title, x_title) {
  outcome_data <- plot_data[
    plot_data$Outcome == outcome_name,
    ,
    drop = FALSE
  ]

  ggplot(outcome_data) +
    geom_blank(
      data = axis_limits,
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
        linetype = Base
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
        linetype = Base
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
        linetype = Base
      ),
      colour = "#222222",
      linewidth = 0.46
    ) +
    geom_point(
      aes(
        x = Estimate,
        y = Y,
        shape = Base,
        fill = Base
      ),
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
    facet_grid(
      . ~ Lesion,
      scales = "free_x",
      space = "fixed"
    ) +
    scale_y_continuous(
      breaks = c(1, 2),
      labels = c(
        "Literature-based FIT\nsensitivity",
        "PRENEC-derived FIT\nsensitivity"
      ),
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
      values = c(
        "A" = 21,
        "B" = 21
      ),
      labels = c("Base A", "Base B")
    ) +
    scale_fill_manual(
      name = "Analysis base",
      values = c(
        "A" = "#111111",
        "B" = "white"
      ),
      labels = c("Base A", "Base B")
    ) +
    scale_linetype_manual(
      name = "Analysis base",
      values = c(
        "A" = "solid",
        "B" = "longdash"
      ),
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
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.title = element_text(size = 7.7, face = "bold"),
      legend.text = element_text(size = 7.7),
      legend.key.width = grid::unit(5, "mm"),
      legend.spacing.x = grid::unit(1.5, "mm"),
      plot.margin = margin(3, 5, 3, 5)
    )
}

adjusted_panel <- create_outcome_panel(
  "Adjusted population detection rate",
  "(A) Adjusted population detection among both sexes",
  "Adjusted population detection rate (%)"
) +
  theme(legend.position = "none")

prevalence_panel <- create_outcome_panel(
  "Modelled prevalence",
  "(B) Modelled prevalence among both sexes",
  "Modelled prevalence (%)"
) +
  theme(strip.text.x = element_blank())

# A dedicated spacer separates the two outcome rows while preserving aligned
# lesion columns and a single legend for the complete figure.
figure <- (adjusted_panel / plot_spacer() / prevalence_panel) +
  plot_layout(heights = c(1, 0.13, 1))

output_dir <- file.path(
  publication_output_directory,
  "figures"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

width_mm <- 190
height_mm <- 138
file_stem <- "figure_adjusted_detection_prevalence_adherence_sampled"

ggsave(
  filename = file.path(output_dir, paste0(file_stem, ".pdf")),
  plot = figure,
  device = grDevices::cairo_pdf,
  width = width_mm,
  height = height_mm,
  units = "mm"
)
ggsave(
  filename = file.path(output_dir, paste0(file_stem, ".png")),
  plot = figure,
  device = ragg::agg_png,
  width = width_mm,
  height = height_mm,
  units = "mm",
  dpi = 600
)
ggsave(
  filename = file.path(output_dir, paste0(file_stem, ".tiff")),
  plot = figure,
  device = ragg::agg_tiff,
  width = width_mm,
  height = height_mm,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)
ggsave(
  filename = file.path(output_dir, paste0(file_stem, ".svg")),
  plot = figure,
  device = svglite::svglite,
  width = width_mm / 25.4,
  height = height_mm / 25.4,
  units = "in"
)

plot_data <- plot_data[order(
  plot_data$Outcome,
  plot_data$Lesion,
  plot_data$Scenario,
  plot_data$Base
), ]

write.table(
  plot_data[c(
    "Outcome", "Lesion", "Scenario", "Base", "Estimate", "Lower", "Upper"
  )],
  file = file.path(output_dir, "figure_data.tsv"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

message(
  "Publication figure written to: ",
  normalizePath(output_dir, winslash = "/", mustWork = TRUE)
)

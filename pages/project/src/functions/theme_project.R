# theme_project.R
# ------------------------------------------------------------------------------
# Purpose : One shared look for every chart in the project: theme, colors,
#           axis-label helpers, and a helper that saves each figure.
# Lives in: pages/project/src/functions/theme_project.R
# Used by : pages/project/page01.qmd (loaded with source())
# ------------------------------------------------------------------------------

library(ggplot2)

# ---- Colors ------------------------------------------------------------------
# Blue and vermillion come from the Okabe-Ito colorblind-safe palette.
# Color always means the same thing in this project:
#   blue       = stock market larger than expected for the income level
#   vermillion = stock market smaller than expected
#   gray       = every other economy (context)

project_colors <- c(
  above = "#0072B2",
  below = "#D55E00",
  other = "#B4B4B4",
  ink   = "#222222",
  muted = "#6B6B6B",
  grid  = "#E6E6E6"
)

group_levels <- c("Above expected", "Below expected", "Other economies")

group_colors <- c(
  "Above expected"  = project_colors[["above"]],
  "Below expected"  = project_colors[["below"]],
  "Other economies" = project_colors[["other"]]
)

# ---- Theme -------------------------------------------------------------------

theme_project <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", size = rel(1.15), color = project_colors[["ink"]]),
      plot.subtitle = element_text(color = project_colors[["muted"]], margin = margin(b = 12)),
      plot.caption  = element_text(color = project_colors[["muted"]], size = rel(0.75),
                                   hjust = 0, margin = margin(t = 12)),
      axis.title    = element_text(color = project_colors[["muted"]], size = rel(0.9)),
      axis.text     = element_text(color = project_colors[["ink"]]),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = project_colors[["grid"]], linewidth = 0.3),
      strip.text    = element_text(face = "bold", hjust = 0, color = project_colors[["ink"]]),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      plot.margin = margin(12, 16, 10, 12)
    )
}

# ---- Axis-label helpers ------------------------------------------------------

# 150 -> "150%", 1000 -> "1,000%", 0.3 -> "0.3%"
label_pct <- function(x) {
  ifelse(is.na(x), NA_character_, paste0(prettyNum(x, big.mark = ","), "%"))
}

# Ratio of actual to expected: 2 -> "2×", 0.25 -> "0.25×", 22.47 -> "22×"
label_times <- function(x) {
  ifelse(is.na(x), NA_character_, paste0(prettyNum(signif(x, 2), big.mark = ","), "×"))
}

# Shared axis breaks for log scales.
pct_breaks        <- c(0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000, 3000)
ratio_breaks      <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5,
                       1, 2, 5, 10, 20, 50, 100)
ratio_breaks_wide <- c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100)

# Compact value labels for charts: 302.9 -> "303", 0.0831 -> "0.083"
label_value <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, prettyNum(signif(x, digits), big.mark = ","))
}

# ---- Save and show a figure --------------------------------------------------
# Saves a PNG to pages/project/figs/ and returns the plot so it also appears
# on the page.

show_and_save <- function(plot, name, width = 8, height = 5.5) {
  figure_dir <- here::here("pages", "project", "figs")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  ggsave(
    filename = file.path(figure_dir, paste0(name, ".png")),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = 300,
    bg       = "white"
  )

  plot
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(svglite)
})

excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/未来情景/coal_plant_future_result2.xlsx"
out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图三退役与未来情景/2050年三情景避免损失环形图"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
FIG3_STYLE <- Sys.getenv("FIG3_STYLE", unset = "purple")
style_suffix <- if (FIG3_STYLE == "fig2_harmonized") "_fig2_palette" else ""

scenario_labels <- c(
  SSP126 = "SSP1-2.6",
  SSP245 = "SSP2-4.5",
  SSP370 = "SSP3-7.0"
)

raw <- read_excel(excel_path)

annular_sector <- function(start, end, outer = 0.92, inner = 0.58, n = 360) {
  theta_outer <- seq(start, end, length.out = n)
  theta_inner <- rev(theta_outer)
  data.frame(
    x = c(outer * cos(theta_outer), inner * cos(theta_inner)),
    y = c(outer * sin(theta_outer), inner * sin(theta_inner))
  )
}

make_donut <- function(scenario_code) {
  baseline_col <- paste0("loss_", scenario_code, "_2021")
  future_col <- paste0("loss_", scenario_code, "_2050")
  if (!all(c(baseline_col, future_col) %in% names(raw))) {
    stop("Missing scenario columns for ", scenario_code)
  }

  baseline <- suppressWarnings(as.numeric(raw[[baseline_col]]))
  future <- suppressWarnings(as.numeric(raw[[future_col]]))
  valid <- is.finite(baseline) & is.finite(future) & baseline >= 0
  baseline_total <- sum(baseline[valid], na.rm = TRUE)
  future_total <- sum(pmin(pmax(future[valid], 0), baseline[valid]), na.rm = TRUE)
  avoided_total <- baseline_total - future_total
  avoided_share <- avoided_total / baseline_total

  start_angle <- pi / 2
  avoided_end <- start_angle - 2 * pi * avoided_share
  residual_end <- start_angle - 2 * pi
  avoided_poly <- transform(
    annular_sector(start_angle, avoided_end), component = "Avoided"
  )
  residual_poly <- transform(
    annular_sector(avoided_end, residual_end), component = "Residual"
  )
  pie_data <- bind_rows(avoided_poly, residual_poly)

  avoided_col <- if (FIG3_STYLE == "fig2_harmonized") "#315B73" else "#65417E"
  residual_col <- if (FIG3_STYLE == "fig2_harmonized") "#DDE6E5" else "#E4DEE8"
  text_col <- if (FIG3_STYLE == "fig2_harmonized") "#315B73" else "#4D2E68"

  p <- ggplot(pie_data, aes(x, y, group = component, fill = component)) +
    geom_polygon(colour = "white", linewidth = 0.55) +
    annotate(
      "text", x = 0, y = 0.10,
      label = sprintf("%.1f%%", 100 * avoided_share),
      colour = text_col, fontface = "bold", size = 5.4
    ) +
    annotate(
      "text", x = 0, y = -0.13,
      label = "avoided",
      colour = text_col, fontface = "bold", size = 3.2
    ) +
    annotate(
      "text", x = 0, y = -1.10,
      label = sprintf("%.2f B USD yr^-1", avoided_total / 1e9),
      colour = "#3E4244", fontface = "bold", size = 3.1
    ) +
    scale_fill_manual(values = c(Avoided = avoided_col, Residual = residual_col)) +
    coord_equal(xlim = c(-1.05, 1.05), ylim = c(-1.27, 1.02), clip = "off") +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.margin = margin(0, 0, 0, 0)
    )

  stem <- file.path(
    out_dir,
    paste0("avoided_loss_donut_", scenario_code, "_2050", style_suffix)
  )
  ggsave(paste0(stem, ".svg"), p, width = 3.0, height = 2.25,
         device = svglite, bg = "transparent")
  ggsave(paste0(stem, ".pdf"), p, width = 3.0, height = 2.25,
         bg = "transparent")

  tibble(
    scenario = scenario_labels[[scenario_code]],
    baseline_busd = baseline_total / 1e9,
    future_busd = future_total / 1e9,
    avoided_busd = avoided_total / 1e9,
    avoided_share = avoided_share
  )
}

summary_df <- bind_rows(lapply(names(scenario_labels), make_donut))
write.csv(
  summary_df,
  file.path(out_dir, paste0("avoided_loss_donut_summary", style_suffix, ".csv")),
  row.names = FALSE
)
print(summary_df)

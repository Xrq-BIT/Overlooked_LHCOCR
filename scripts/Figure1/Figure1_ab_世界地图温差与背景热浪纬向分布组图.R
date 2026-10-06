suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(scales)
  library(cowplot)
  library(svglite)
  library(grid)
})

# Default behavior reproduces the Delta-HWN version. Set
# FIG1_HEAT_METRIC=mmt_exceedance to create the separate MMT version.
heat_mode <- Sys.getenv("FIG1_HEAT_METRIC", unset = "delta_hwn")
if (heat_mode %in% c("mmt_exceedance", "mmt_background")) {
  excel_path <- paste0(
    "/Users/xuruiqing/Desktop/文章试验整体流程/0706专注煤电与微气候反事实/",
    "coal_plants_with_LSM_10km_productivity副本2_MMT_ERA5_delt_warm_min.xlsx"
  )
  out_dir <- paste0(
    "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图一温差统计/",
    ifelse(
      heat_mode == "mmt_exceedance",
      "dT_MMT_exceedance_latitudinal_figure",
      "dT_MMT_background_latitudinal_figure"
    )
  )
} else {
  excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/0706专注煤电与微气候反事实/coal_plants_with_LSM_10km_productivity副本2.xlsx"
  out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图一温差统计/dT_HWN_latitudinal_figure"
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

LON_COL <- "Longitude"
LAT_COL <- "Latitude"
DT_COL <- "delt_warm_min"
DELTA_HWN_COLS <- paste0("Delta_HWN_", 2011:2020)
MMT_DELTA_COL <- "Delta_MMT_days_mean_2011_2020"
MMT_BACKGROUND_COL <- "MMT_days_without_mean_2011_2020"

OUT_DT_MAP_PDF <- file.path(out_dir, "a_map_local_dT.pdf")
OUT_DT_MAP_SVG <- file.path(out_dir, "a_map_local_dT.svg")
heat_tag <- if (
  heat_mode == "mmt_exceedance"
) "delta_MMT_days" else if (
  heat_mode == "mmt_background"
) "background_MMT_days" else "delta_HWN_local_dT"
OUT_HWN_MAP_PDF <- file.path(out_dir, paste0("b_map_", heat_tag, ".pdf"))
OUT_HWN_MAP_SVG <- file.path(out_dir, paste0("b_map_", heat_tag, ".svg"))
OUT_DT_PROFILE_PDF <- file.path(out_dir, "c_latitudinal_profile_local_dT.pdf")
OUT_DT_PROFILE_SVG <- file.path(out_dir, "c_latitudinal_profile_local_dT.svg")
OUT_HWN_PROFILE_PDF <- file.path(out_dir, paste0("d_latitudinal_profile_", heat_tag, ".pdf"))
OUT_HWN_PROFILE_SVG <- file.path(out_dir, paste0("d_latitudinal_profile_", heat_tag, ".svg"))
final_tag <- if (
  heat_mode == "mmt_exceedance"
) "MMT_exceedance" else if (
  heat_mode == "mmt_background"
) "MMT_background" else "HWN"
OUT_FINAL_PDF <- file.path(out_dir, paste0("Fig1_dT_", final_tag, "_maps_latitudinal_profiles.pdf"))
OUT_FINAL_SVG <- file.path(out_dir, paste0("Fig1_dT_", final_tag, "_maps_latitudinal_profiles.svg"))
OUT_PROFILE_CSV <- file.path(out_dir, "latitudinal_profiles_10deg_mean_sd.csv")

# -------------------------
# Data
# -------------------------
df_raw <- read_excel(excel_path)
heat_input_cols <- if (
  heat_mode == "mmt_exceedance"
) MMT_DELTA_COL else if (
  heat_mode == "mmt_background"
) MMT_BACKGROUND_COL else DELTA_HWN_COLS
need_cols <- c(LON_COL, LAT_COL, DT_COL, heat_input_cols)
if (!all(need_cols %in% names(df_raw))) {
  stop("缺少必要列：", paste(setdiff(need_cols, names(df_raw)), collapse = ", "))
}

df <- df_raw %>%
  mutate(
    longitude = suppressWarnings(as.numeric(.data[[LON_COL]])),
    latitude = suppressWarnings(as.numeric(.data[[LAT_COL]])),
    local_dT = suppressWarnings(as.numeric(.data[[DT_COL]]))
  ) %>%
  filter(!is.na(longitude), !is.na(latitude))

if (heat_mode == "mmt_exceedance") {
  df <- df %>%
    mutate(heat_metric = suppressWarnings(as.numeric(.data[[MMT_DELTA_COL]])))
} else if (heat_mode == "mmt_background") {
  df <- df %>%
    mutate(heat_metric = suppressWarnings(as.numeric(.data[[MMT_BACKGROUND_COL]])))
} else {
  df <- df %>%
    mutate(
      across(all_of(DELTA_HWN_COLS), ~ suppressWarnings(as.numeric(.x))),
      heat_metric = rowMeans(across(all_of(DELTA_HWN_COLS)), na.rm = TRUE),
      heat_metric = if_else(is.nan(heat_metric), NA_real_, heat_metric)
    )
}

df_dt <- df %>% filter(!is.na(local_dT))
if (heat_mode == "mmt_background") {
  df_hwn <- df %>% filter(!is.na(heat_metric), heat_metric >= 0)
} else {
  df_hwn <- df %>%
    filter(!is.na(heat_metric), !is.na(local_dT), local_dT >= 0, heat_metric >= 0)
}
world <- ne_countries(scale = "medium", returnclass = "sf")

# -------------------------
# Shared map style
# -------------------------
longitude_labels <- function(x) {
  paste0(
    ifelse(x == 0, "0", abs(x)), "\u00B0",
    ifelse(x < 0, "W", ifelse(x > 0, "E", ""))
  )
}

latitude_labels <- function(y) {
  paste0(
    ifelse(y == 0, "0", abs(y)), "\u00B0",
    ifelse(y < 0, "S", ifelse(y > 0, "N", ""))
  )
}

map_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey25", size = 8.5),
    axis.ticks = element_line(color = "grey60", linewidth = 0.3),
    axis.ticks.length = unit(1.8, "mm"),
    panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.45),
    plot.margin = margin(4, 6, 4, 6)
  )

base_map <- ggplot() +
  geom_sf(data = world, fill = "#F0F1EF", color = "#C9CCCA", linewidth = 0.22) +
  coord_sf(expand = FALSE, xlim = c(-150, 150), ylim = c(-60, 80)) +
  scale_x_continuous(
    breaks = seq(-150, 150, by = 60),
    labels = longitude_labels
  ) +
  scale_y_continuous(
    breaks = seq(-30, 60, by = 30),
    labels = latitude_labels
  ) +
  map_theme

# -------------------------
# Panel a: original local-dT map style
# -------------------------
dt_limits <- c(-0.5, 1.0)
dt_cols <- c("#214C6B", "#5F86A0", "#B8CCD6", "#F2EFEA", "#E4B49F", "#C97860", "#984A3A")
dt_anchors <- c(-0.5, -0.20, -0.05, 0.00, 0.20, 0.60, 1.00)
dt_values <- rescale(dt_anchors, to = c(0, 1), from = dt_limits)
dt_breaks <- c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0)

dt_scale <- scale_color_gradientn(
  colours = dt_cols,
  values = dt_values,
  limits = dt_limits,
  oob = squish,
  breaks = dt_breaks,
  labels = label_number(accuracy = 0.01),
  name = "Local dT (degC)"
)

p_dt_core <- base_map +
  geom_point(
    data = df_dt,
    aes(x = longitude, y = latitude, color = local_dT),
    size = 0.62,
    alpha = 0.95
  ) +
  geom_rect(
    data = data.frame(
      panel = c("e", "f", "g"),
      xmin = c(107.5, 28.3, 80.0),
      xmax = c(125.5, 46.3, 98.0),
      ymin = c(26.0, 31.5, 37.3),
      ymax = c(41.0, 46.5, 52.3)
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = NA,
    color = "#4F78A8",
    linewidth = 0.65
  ) +
  geom_text(
    data = data.frame(
      panel = c("e", "f", "g"),
      x = c(108.1, 28.9, 80.6),
      y = c(26.7, 32.2, 38.0)
    ),
    aes(x = x, y = y, label = panel),
    inherit.aes = FALSE,
    color = "#3F648F",
    size = 3.0,
    fontface = "bold",
    hjust = 0,
    vjust = 0
  ) +
  dt_scale

dt_legend <- cowplot::get_legend(
  p_dt_core +
    guides(color = guide_colorbar(
      barheight = unit(39, "mm"),
      barwidth = unit(5.5, "mm"),
      ticks.colour = "grey30",
      frame.colour = "grey30"
    )) +
    theme(
      legend.position = "left",
      legend.title = element_text(size = 9.2),
      legend.text = element_text(size = 8.2)
    )
)

p_dt_map <- cowplot::ggdraw() +
  draw_plot(p_dt_core + theme(legend.position = "none")) +
  draw_grob(dt_legend, x = 0.055, y = 0.03, width = 0.18, height = 0.54, scale = 0.43)

# -------------------------
# Panel b: mean annual Delta HWN caused by local ring-scale warming
# -------------------------
if (heat_mode == "mmt_exceedance") {
  hwn_limits <- c(0, 30)
  hwn_breaks <- c(0, 10, 20, 30)
  hwn_knots <- c(0, 1, 3, 6, 10, 20, 30)
  hwn_cols <- c("#F2EFEA", "#EED8C9", "#E3B59E", "#D98F73", "#C9634D", "#AD4E40", "#7F3A35")
  heat_legend_title <- "Added days above MMT\n(days yr-1)"
  heat_legend_labels <- c("0", "10", "20", "30+")
  heat_profile_label <- "Added days above MMT\n(days yr-1)"
  heat_metric_name <- "Mean annual added MMT-exceedance days"
} else if (heat_mode == "mmt_background") {
  hwn_limits <- c(0, 220)
  hwn_breaks <- c(0, 50, 100, 150, 220)
  hwn_knots <- c(0, 25, 50, 80, 120, 170, 220)
  hwn_cols <- c("#F2EFEA", "#EED8C9", "#E3B59E", "#D98F73", "#C9634D", "#AD4E40", "#7F3A35")
  heat_legend_title <- "Background days above MMT\n(days yr-1)"
  heat_legend_labels <- c("0", "50", "100", "150", "220+")
  heat_profile_label <- "Background days above MMT\n(days yr-1)"
  heat_metric_name <- "Mean annual background MMT-exceedance days"
} else {
  hwn_limits <- c(0, 20)
  # Keep the original colour interpolation anchors so point colours do not
  # change. The duplicated endpoint keeps values from 16 to 20 at the same
  # darkest colour while allowing regular 0/10/20 legend ticks.
  hwn_breaks <- c(0, 10, 20)
  hwn_knots <- c(0, 1, 3, 6, 10, 16, 20)
  hwn_cols <- c("#F2EFEA", "#EED8C9", "#E3B59E", "#D98F73", "#C9634D", "#984A3A", "#984A3A")
  heat_legend_title <- "Mean annual \u0394HWN\n(days yr-1)"
  heat_legend_labels <- c("0", "10", "20")
  heat_profile_label <- "Mean annual \u0394HWN\n(days yr-1)"
  heat_metric_name <- "Mean annual Delta HWN"
}

hwn_scale <- scale_color_gradientn(
  colours = hwn_cols,
  values = rescale(hwn_knots, from = hwn_limits),
  limits = hwn_limits,
  oob = squish,
  breaks = hwn_breaks,
  labels = heat_legend_labels,
  name = heat_legend_title
)

p_hwn_core <- base_map +
  geom_point(
    data = df_hwn,
    aes(x = longitude, y = latitude, color = heat_metric),
    size = 0.62,
    alpha = 0.95
  ) +
  geom_rect(
    data = data.frame(
      panel = c("c", "d", "e", "f"),
      xmin = c(107.5, 28.3, 80.0, -96.5),
      xmax = c(125.5, 46.3, 98.0, -78.5),
      ymin = c(26.0, 31.5, 37.3, 31.6),
      ymax = c(41.0, 46.5, 52.3, 46.6)
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = NA,
    color = "#4F78A8",
    linewidth = 0.65
  ) +
  geom_text(
    data = data.frame(
      panel = c("c", "d", "e", "f"),
      x = c(108.1, 28.9, 80.6, -95.9),
      y = c(26.7, 32.2, 38.0, 32.3)
    ),
    aes(x = x, y = y, label = panel),
    inherit.aes = FALSE,
    color = "#3F648F",
    size = 3.0,
    fontface = "bold",
    hjust = 0,
    vjust = 0
  ) +
  hwn_scale

hwn_legend <- cowplot::get_legend(
  p_hwn_core +
    guides(color = guide_colorbar(
      barheight = unit(39, "mm"),
      barwidth = unit(5.5, "mm"),
      ticks.colour = "grey30",
      frame.colour = "grey30"
    )) +
    theme(
      legend.position = "left",
      legend.title = element_text(size = 9.2),
      legend.text = element_text(size = 8.2)
    )
)

p_hwn_map <- cowplot::ggdraw() +
  draw_plot(p_hwn_core + theme(legend.position = "none")) +
  draw_grob(
    hwn_legend,
    x = ifelse(
      heat_mode == "mmt_exceedance", 0.078,
      ifelse(heat_mode == "mmt_background", 0.093, 0.074)
    ),
    y = 0.03, width = 0.18, height = 0.54, scale = 0.43
  )

# -------------------------
# Panels c/d: 10-degree latitudinal mean and one standard deviation
# -------------------------
lat_step <- 10
lat_breaks <- seq(-60, 80, by = lat_step)

make_lat_profile <- function(data, value_col, metric_name) {
  data %>%
    filter(latitude >= min(lat_breaks), latitude < max(lat_breaks)) %>%
    mutate(
      lat_index = findInterval(latitude, lat_breaks, rightmost.closed = FALSE),
      latitude_center = lat_breaks[lat_index] + lat_step / 2
    ) %>%
    filter(!is.na(latitude_center), !is.na(.data[[value_col]])) %>%
    group_by(latitude_center) %>%
    summarise(
      latitude = first(latitude_center),
      n = n(),
      mean = mean(.data[[value_col]]),
      sd = sd(.data[[value_col]]),
      q25 = quantile(.data[[value_col]], 0.25),
      median = median(.data[[value_col]]),
      q75 = quantile(.data[[value_col]], 0.75),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    mutate(metric = metric_name) %>%
    arrange(latitude)
}

profile_dt <- make_lat_profile(df_dt, "local_dT", "Local dT")
profile_hwn <- make_lat_profile(df_hwn, "heat_metric", heat_metric_name)
profile_hwn <- profile_hwn %>%
  mutate(ribbon_min = pmax(mean - sd, 0), ribbon_max = mean + sd)
write.csv(bind_rows(profile_dt, profile_hwn), OUT_PROFILE_CSV, row.names = FALSE)

profile_theme <- theme_classic(base_size = 10) +
  theme(
    panel.border = element_rect(fill = NA, color = "#555555", linewidth = 0.4),
    axis.line = element_blank(),
    axis.text = element_text(size = 8.2, color = "grey25"),
    axis.title = element_text(size = 9.3, color = "black"),
    axis.ticks = element_line(color = "grey45", linewidth = 0.3),
    plot.margin = margin(5, 28, 5, 5)
  )

p_dt_profile <- ggplot(profile_dt, aes(y = latitude)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_ribbon(
    aes(xmin = mean - sd, xmax = mean + sd),
    fill = "#D9DDDC",
    alpha = 0.62,
    orientation = "y"
  ) +
  geom_path(aes(x = mean), color = "#B85F4B", linewidth = 0.85) +
  scale_y_continuous(
    limits = c(-60, 80),
    breaks = c(-30, 0, 30, 60),
    labels = latitude_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  labs(x = "Local dT (degC)", y = "Latitude") +
  profile_theme

p_hwn_profile <- ggplot(profile_hwn, aes(y = latitude)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_ribbon(
    aes(xmin = ribbon_min, xmax = ribbon_max),
    fill = "#D9DDDC",
    alpha = 0.62,
    orientation = "y"
  ) +
  geom_path(aes(x = mean), color = "#B85F4B", linewidth = 0.85) +
  scale_y_continuous(
    limits = c(-60, 80),
    breaks = c(-30, 0, 30, 60),
    labels = latitude_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  labs(x = heat_profile_label, y = "Latitude") +
  profile_theme

# -------------------------
# Export individual panels and 2 x 2 composition
# -------------------------
ggsave(OUT_DT_MAP_SVG, p_dt_map, width = 10.1, height = 4.35, device = svglite::svglite)
ggsave(OUT_DT_MAP_PDF, p_dt_map, width = 10.1, height = 4.35, device = grDevices::pdf)
ggsave(OUT_HWN_MAP_SVG, p_hwn_map, width = 10.1, height = 4.35, device = svglite::svglite)
ggsave(OUT_HWN_MAP_PDF, p_hwn_map, width = 10.1, height = 4.35, device = grDevices::pdf)
ggsave(OUT_DT_PROFILE_SVG, p_dt_profile, width = 2.55, height = 4.35, device = svglite::svglite)
ggsave(OUT_DT_PROFILE_PDF, p_dt_profile, width = 2.55, height = 4.35, device = grDevices::pdf)
ggsave(OUT_HWN_PROFILE_SVG, p_hwn_profile, width = 2.55, height = 4.35, device = svglite::svglite)
ggsave(OUT_HWN_PROFILE_PDF, p_hwn_profile, width = 2.55, height = 4.35, device = grDevices::pdf)

row_top <- plot_grid(
  p_dt_map,
  p_dt_profile,
  nrow = 1,
  rel_widths = c(4.55, 1.15)
)

row_bottom <- plot_grid(
  p_hwn_map,
  p_hwn_profile,
  nrow = 1,
  rel_widths = c(4.55, 1.15)
)

p_final <- plot_grid(row_top, row_bottom, ncol = 1, rel_heights = c(1, 1))

ggsave(OUT_FINAL_SVG, p_final, width = 12.8, height = 8.9, device = svglite::svglite)
ggsave(OUT_FINAL_PDF, p_final, width = 12.8, height = 8.9, device = grDevices::pdf)

message(
  "Done\n",
  "- ", OUT_FINAL_PDF, "\n",
  "- ", OUT_FINAL_SVG, "\n",
  "- ", OUT_PROFILE_CSV
)

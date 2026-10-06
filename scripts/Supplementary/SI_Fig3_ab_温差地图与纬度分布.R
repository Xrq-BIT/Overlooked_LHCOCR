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

args <- grep("^--file=", commandArgs(FALSE), value=TRUE)
folder <- if(length(args)) dirname(normalizePath(sub("^--file=", "", args[1]))) else getwd()
out_dir <- file.path(folder,"reproduced")
dir.create(out_dir,showWarnings=FALSE)
excel_path <- file.path(folder,"source_data/Source_Data_SI_Fig3.xlsx")
LON_COL <- "Longitude"; LAT_COL <- "Latitude"; DT_COL <- "delt_warm_min"
df_raw <- read_excel(excel_path)
df <- df_raw %>%
  mutate(
    longitude = suppressWarnings(as.numeric(.data[[LON_COL]])),
    latitude = suppressWarnings(as.numeric(.data[[LAT_COL]])),
    local_dT = suppressWarnings(as.numeric(.data[[DT_COL]]))
  ) %>%
  filter(!is.na(longitude), !is.na(latitude))

df_dt <- df %>% filter(!is.na(local_dT))
world <- ne_countries(scale="medium",returnclass="sf")
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


ggsave(file.path(out_dir,"SI_Fig3_a.svg"),p_dt_map,width=10.1,height=4.35,device=svglite::svglite)
ggsave(file.path(out_dir,"SI_Fig3_b.svg"),p_dt_profile,width=2.55,height=4.35,device=svglite::svglite)
# Final SI removed case locators and shifted the complete map legend right.
doc <- xml2::read_xml(file.path(out_dir,"SI_Fig3_a.svg"))
nodes <- xml2::xml_find_all(doc,"//*[local-name()='rect' and contains(@style,'stroke: #4F78A8')] | //*[local-name()='text' and contains(@style,'fill: #3F648F')]")
xml2::xml_remove(nodes)
title <- xml2::xml_find_first(doc,"//*[local-name()='text' and text()='Local dT (degC)']")
for(n in c(list(title),as.list(xml2::xml_find_all(title,'following-sibling::*')))) xml2::xml_set_attr(n,'transform','translate(22 0)')
xml2::write_xml(doc,file.path(out_dir,"SI_Fig3_a.svg"))
for(letter in c("a","b")) rsvg::rsvg_pdf(file.path(out_dir,paste0("SI_Fig3_",letter,".svg")),file.path(out_dir,paste0("SI_Fig3_",letter,".pdf")))
write.csv(profile_dt,file.path(out_dir,"SI_Fig3_b_latitude_summary.csv"),row.names=FALSE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(scales)
  library(stringr)
  library(DBI)
  library(RSQLite)
  library(svglite)
  library(rsvg)
})

sf::sf_use_s2(FALSE)

# Set SCENARIO=SSP245 or SCENARIO=SSP370 when invoking Rscript. The default
# remains SSP126 so the script is directly reproducible without arguments.
SCENARIO <- Sys.getenv("SCENARIO", unset = "SSP126")
FIG3_STYLE <- Sys.getenv("FIG3_STYLE", unset = "purple")
YEAR_USE <- 2050

excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/未来情景/coal_plant_future_result2.xlsx"
gadm_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/data/gadm_410.gpkg"
out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图三退役与未来情景/2050年GADM城市年度损失减少世界地图"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

scenario_label <- c(
  SSP126 = "SSP1-2.6",
  SSP245 = "SSP2-4.5",
  SSP370 = "SSP3-7.0"
)[[SCENARIO]]
future_loss_col <- paste0("loss_", SCENARIO, "_", YEAR_USE)

style_suffix <- if (FIG3_STYLE == "fig2_harmonized") "_fig2_palette" else ""
out_stem <- paste0(
  "gadm_admin2_annual_loss_reduction_", SCENARIO, "_", YEAR_USE,
  style_suffix
)
out_svg <- file.path(out_dir, paste0(out_stem, ".svg"))
out_pdf <- file.path(out_dir, paste0(out_stem, ".pdf"))
out_png <- file.path(out_dir, paste0(out_stem, ".png"))
out_csv <- file.path(out_dir, paste0(out_stem, "_summary.csv"))

clean_name <- function(x) str_squish(str_trim(as.character(x)))
valid_name <- function(x) {
  !is.na(x) & x != "" &
    !str_to_lower(x) %in% c("unknown", "na", "n/a", "none", "null")
}
lon_label <- function(x) {
  paste0(ifelse(x == 0, "0", abs(x)), "\u00b0", ifelse(x < 0, "W", ifelse(x > 0, "E", "")))
}
lat_label <- function(y) {
  paste0(ifelse(y == 0, "0", abs(y)), "\u00b0", ifelse(y < 0, "S", ifelse(y > 0, "N", "")))
}

df_raw <- read_excel(excel_path)
need_cols <- c(
  "Longitude", "Latitude", "gadm_iso3", "gadm_admin1", "gadm_admin2",
  "All_loss_annual_mean_LSM_10km", "future_loss_analysis_included_2021",
  future_loss_col
)
if (!all(need_cols %in% names(df_raw))) {
  stop("Missing columns: ", paste(setdiff(need_cols, names(df_raw)), collapse = ", "))
}

# Only plants admitted to the 2021 retirement/loss analysis are retained.
plant_df <- df_raw %>%
  transmute(
    lon = suppressWarnings(as.numeric(Longitude)),
    lat = suppressWarnings(as.numeric(Latitude)),
    geometry_iso3 = str_to_upper(clean_name(gadm_iso3)),
    admin1 = clean_name(gadm_admin1),
    admin2 = clean_name(gadm_admin2),
    included = suppressWarnings(as.numeric(future_loss_analysis_included_2021)),
    baseline_loss_usd = suppressWarnings(as.numeric(All_loss_annual_mean_LSM_10km)),
    future_loss_usd = suppressWarnings(as.numeric(.data[[future_loss_col]]))
  ) %>%
  filter(
    included == 1,
    is.finite(lon), is.finite(lat),
    valid_name(geometry_iso3), valid_name(admin1), valid_name(admin2),
    is.finite(baseline_loss_usd), baseline_loss_usd > 0,
    is.finite(future_loss_usd)
  ) %>%
  mutate(
    future_loss_usd = pmin(pmax(future_loss_usd, 0), baseline_loss_usd),
    avoided_loss_usd = pmax(baseline_loss_usd - future_loss_usd, 0)
  )

# Administrative-city reduction is a sum of plant-level avoided annual losses.
city_stat <- plant_df %>%
  group_by(geometry_iso3, admin1, admin2) %>%
  summarise(
    fallback_lon = mean(lon, na.rm = TRUE),
    fallback_lat = mean(lat, na.rm = TRUE),
    baseline_loss_usd = sum(baseline_loss_usd, na.rm = TRUE),
    future_loss_usd = sum(future_loss_usd, na.rm = TRUE),
    avoided_loss_usd = sum(avoided_loss_usd, na.rm = TRUE),
    avoided_loss_musd = avoided_loss_usd / 1e6,
    plant_count = n(),
    retired_plant_count = sum(avoided_loss_usd > 0),
    .groups = "drop"
  ) %>%
  filter(avoided_loss_usd > 1e-6)

# Resolve Excel GADM names to GID_2, then use the administrative polygon's
# point-on-surface. Plant mean coordinates are retained only as a fallback.
con <- dbConnect(SQLite(), gadm_path)
on.exit(dbDisconnect(con), add = TRUE)
gadm_index <- dbGetQuery(
  con,
  paste(
    "SELECT DISTINCT GID_0, NAME_1, GID_2, NAME_2",
    "FROM gadm_410 WHERE GID_2 IS NOT NULL AND GID_2 <> ''"
  )
) %>%
  transmute(
    geometry_iso3 = str_to_upper(clean_name(GID_0)),
    admin1 = clean_name(NAME_1),
    admin2 = clean_name(NAME_2),
    GID_2 = as.character(GID_2)
  )

exact_index <- gadm_index %>%
  distinct(geometry_iso3, admin1, admin2, .keep_all = TRUE)
unique_admin2_index <- gadm_index %>%
  group_by(geometry_iso3, admin2) %>%
  filter(n_distinct(GID_2) == 1) %>%
  slice(1) %>%
  ungroup() %>%
  select(geometry_iso3, admin2, GID_2_fallback = GID_2)

city_stat <- city_stat %>%
  left_join(exact_index, by = c("geometry_iso3", "admin1", "admin2")) %>%
  left_join(unique_admin2_index, by = c("geometry_iso3", "admin2")) %>%
  mutate(
    GID_2 = coalesce(GID_2, GID_2_fallback),
    geometry_match = ifelse(is.na(GID_2), "plant_mean_fallback", "gadm_gid2")
  ) %>%
  select(-GID_2_fallback)

matched_gids <- sort(unique(na.omit(city_stat$GID_2)))
if (length(matched_gids) == 0) stop("No GADM admin2 units were matched")

quoted_gids <- dbQuoteString(con, matched_gids)
geometry_query <- paste0(
  "SELECT GID_2, geom FROM gadm_410 WHERE GID_2 IN (",
  paste(quoted_gids, collapse = ","), ")"
)
gadm_parts <- st_read(gadm_path, query = geometry_query, quiet = TRUE)
gadm_admin2 <- gadm_parts %>%
  group_by(GID_2) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  st_make_valid() %>%
  st_transform(4326)

matched_pts <- gadm_admin2 %>%
  inner_join(city_stat %>% filter(!is.na(GID_2)), by = "GID_2") %>%
  st_transform(3857) %>%
  st_point_on_surface() %>%
  st_transform(4326)

fallback_stat <- city_stat %>% filter(is.na(GID_2))
if (nrow(fallback_stat) > 0) {
  fallback_pts <- st_as_sf(
    fallback_stat,
    coords = c("fallback_lon", "fallback_lat"), crs = 4326, remove = FALSE
  )
  city_pts <- bind_rows(matched_pts, fallback_pts)
} else {
  city_pts <- matched_pts
}

write.csv(
  city_stat %>% arrange(desc(avoided_loss_musd)),
  out_csv, row.names = FALSE
)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Use interpretable, evenly spaced legend anchors while retaining continuous
# color interpolation between the highly skewed monetary values.
loss_breaks_real <- c(0, 0.1, 1, 10, 50, 100, 300)
loss_breaks_score <- seq_along(loss_breaks_real)
city_pts <- city_pts %>%
  mutate(
    avoided_loss_cap = pmin(pmax(avoided_loss_musd, 0), max(loss_breaks_real)),
    loss_color_score = approx(
      x = loss_breaks_real,
      y = loss_breaks_score,
      xout = avoided_loss_cap,
      rule = 2
    )$y
  )

map_cols <- if (FIG3_STYLE == "fig2_harmonized") {
  # Harmonized with Figure 2's urban blue-grey family while retaining enough
  # contrast for low and high avoided-loss cities on the pale world basemap.
  c(
    "#B2CBCA", "#91B4B6", "#719DA4", "#568690",
    "#4A7482", "#315B73", "#203F52"
  )
} else {
  c(
    "#CFC0DA", "#B9A1CB", "#9F7FB8", "#835E9F",
    "#684583", "#4D2E68", "#321B49"
  )
}

p <- ggplot() +
  geom_sf(data = world, fill = "#F0F1EF", color = "#C9CCCA", linewidth = 0.24) +
  geom_sf(
    data = city_pts,
    aes(color = loss_color_score),
    size = 1.35, alpha = 0.94
  ) +
  scale_color_gradientn(
    colours = map_cols,
    values = rescale(loss_breaks_score),
    breaks = loss_breaks_score,
    labels = c("0", "0.1", "1", "10", "50", "100", "300"),
    limits = range(loss_breaks_score),
    oob = squish,
    name = "Annual loss reduction\n(M USD)",
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0,
      label.position = "right",
      barheight = unit(42, "mm"), barwidth = unit(5.5, "mm"),
      frame.colour = "grey35", ticks.colour = "grey35"
    )
  ) +
  coord_sf(expand = FALSE, xlim = c(-150, 150), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = seq(-150, 150, 60), labels = lon_label) +
  scale_y_continuous(breaks = seq(-30, 60, 30), labels = lat_label) +
  labs(caption = paste0("Annual loss reduction  |  ", scenario_label, "  |  ", YEAR_USE)) +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.45),
    axis.title = element_blank(),
    axis.text = element_text(color = "#444444", size = 9.5),
    axis.ticks = element_line(color = "grey60", linewidth = 0.3),
    axis.ticks.length = unit(2, "mm"),
    plot.caption = element_text(size = 11.5, face = "bold", hjust = 0.5, color = "#252525"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "inside",
    legend.position.inside = c(0.018, 0.055),
    legend.justification = c(0, 0),
    legend.box = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.90), color = NA),
    legend.margin = margin(3, 4, 3, 4),
    legend.title = element_text(size = 10.5, lineheight = 0.95),
    legend.text = element_text(size = 9.5, color = "grey20"),
    legend.spacing.y = unit(0.5, "mm"),
    plot.margin = margin(3, 5, 3, 5)
  )

ggsave(out_svg, p, width = 12, height = 5.2, device = svglite::svglite)
rsvg_pdf(out_svg, out_pdf)
rsvg_png(out_svg, out_png, width = 3600)

cat("Scenario:", scenario_label, YEAR_USE, "\n")
cat("Included plant records:", nrow(plant_df), "\n")
cat("Cities with positive reduction:", nrow(city_stat), "\n")
cat("GADM centroids:", sum(city_stat$geometry_match == "gadm_gid2"), "\n")
cat("Plant-coordinate fallbacks:", sum(city_stat$geometry_match != "gadm_gid2"), "\n")
cat("Total annual loss reduction (M USD):", round(sum(city_stat$avoided_loss_musd), 3), "\n")
cat("SVG:", out_svg, "\n")
cat("PDF:", out_pdf, "\n")
cat("PNG:", out_png, "\n")
cat("CSV:", out_csv, "\n")

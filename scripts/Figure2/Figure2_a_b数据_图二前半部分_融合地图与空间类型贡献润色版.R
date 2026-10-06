suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(scales)
  library(stringr)
  library(DBI)
  library(RSQLite)
  library(svglite)
  library(cowplot)
})

sf::sf_use_s2(FALSE)

excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/0706专注煤电与微气候反事实/coal_plants_with_LSM_10km_productivity副本2.xlsx"
gadm_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/data/gadm_410.gpkg"
out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图二经济损失/图二前半部分润色版"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_map_svg <- file.path(out_dir, "Fig2a_fused_admin2_loss_smod_map.svg")
out_map_pdf <- file.path(out_dir, "Fig2a_fused_admin2_loss_smod_map.pdf")
out_stack_svg <- file.path(out_dir, "Fig2b_alternative_smod_share_stacked.svg")
out_stack_pdf <- file.path(out_dir, "Fig2b_alternative_smod_share_stacked.pdf")
out_summary <- file.path(out_dir, "Fig2_smod_share_summary.csv")
out_city <- file.path(out_dir, "Fig2_admin2_fused_map_data.csv")

# Ordered, low-saturation palette: urban intensity decreases from dark to light.
smod_levels <- c("Urban core", "Peri-urban", "Rural", "Remote")
smod_pal <- c(
  "Urban core" = "#315B73",
  "Peri-urban" = "#6F9699",
  "Rural" = "#C2A56B",
  "Remote" = "#AAA6A0"
)

clean_name <- function(x) str_squish(str_trim(as.character(x)))
valid_name <- function(x) {
  !is.na(x) & x != "" & !str_to_lower(x) %in% c("unknown", "na", "n/a", "none", "null")
}
recode_smod <- function(x) {
  case_when(
    x == "Urban centre" ~ "Urban core",
    x %in% c("Dense urban cluster", "Semi-dense urban cluster", "Suburban / peri-urban") ~ "Peri-urban",
    x %in% c("Rural cluster", "Low density rural") ~ "Rural",
    TRUE ~ "Remote"
  )
}

raw <- read_excel(excel_path)
needed <- c(
  "Longitude", "Latitude", "All_loss_annual_mean_LSM_10km",
  "pop_10km_total_worldpop2020", "SMOD_2015_label",
  "gadm_iso3", "gadm_admin1", "gadm_admin2"
)
if (!all(needed %in% names(raw))) {
  stop("Missing columns: ", paste(setdiff(needed, names(raw)), collapse = ", "))
}

df <- raw %>%
  transmute(
    lon = suppressWarnings(as.numeric(Longitude)),
    lat = suppressWarnings(as.numeric(Latitude)),
    annual_loss_usd = suppressWarnings(as.numeric(All_loss_annual_mean_LSM_10km)),
    exposed_pop = suppressWarnings(as.numeric(pop_10km_total_worldpop2020)),
    smod_group = recode_smod(as.character(SMOD_2015_label)),
    geometry_iso3 = str_to_upper(clean_name(gadm_iso3)),
    admin1 = clean_name(gadm_admin1),
    admin2 = clean_name(gadm_admin2)
  ) %>%
  filter(
    is.finite(lon), is.finite(lat), is.finite(annual_loss_usd), annual_loss_usd >= 0,
    valid_name(geometry_iso3), valid_name(admin1), valid_name(admin2)
  ) %>%
  mutate(smod_group = factor(smod_group, levels = smod_levels))

# Dominant spatial setting is defined by the largest contribution to city loss.
city_smod <- df %>%
  group_by(geometry_iso3, admin1, admin2, smod_group) %>%
  summarise(smod_loss = sum(annual_loss_usd, na.rm = TRUE), .groups = "drop") %>%
  group_by(geometry_iso3, admin1, admin2) %>%
  arrange(desc(smod_loss), smod_group) %>%
  slice(1) %>%
  ungroup() %>%
  select(geometry_iso3, admin1, admin2, dominant_smod = smod_group)

city_stat <- df %>%
  group_by(geometry_iso3, admin1, admin2) %>%
  summarise(
    fallback_lon = mean(lon),
    fallback_lat = mean(lat),
    annual_total_loss_musd = sum(annual_loss_usd) / 1e6,
    plant_count = n(),
    .groups = "drop"
  ) %>%
  left_join(city_smod, by = c("geometry_iso3", "admin1", "admin2"))

# Match existing admin2 names to GADM geometry and use one point per city.
con <- dbConnect(SQLite(), gadm_path)
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

exact_index <- gadm_index %>% distinct(geometry_iso3, admin1, admin2, .keep_all = TRUE)
fallback_index <- gadm_index %>%
  group_by(geometry_iso3, admin2) %>%
  filter(n_distinct(GID_2) == 1) %>%
  slice(1) %>%
  ungroup() %>%
  select(geometry_iso3, admin2, GID_2_fallback = GID_2)

city_stat <- city_stat %>%
  left_join(exact_index, by = c("geometry_iso3", "admin1", "admin2")) %>%
  left_join(fallback_index, by = c("geometry_iso3", "admin2")) %>%
  mutate(GID_2 = coalesce(GID_2, GID_2_fallback)) %>%
  select(-GID_2_fallback)

matched_gids <- sort(unique(na.omit(city_stat$GID_2)))
quoted <- dbQuoteString(con, matched_gids)
query <- paste0(
  "SELECT GID_2, geom FROM gadm_410 WHERE GID_2 IN (",
  paste(quoted, collapse = ","), ")"
)
dbDisconnect(con)

gadm_parts <- st_read(gadm_path, query = query, quiet = TRUE) %>%
  group_by(GID_2) %>%
  summarise(do_union = TRUE, .groups = "drop") %>%
  st_make_valid() %>%
  st_transform(4326)

matched_pts <- gadm_parts %>%
  inner_join(city_stat %>% filter(!is.na(GID_2)), by = "GID_2") %>%
  st_transform(3857) %>%
  st_point_on_surface() %>%
  st_transform(4326)

fallback_stat <- city_stat %>% filter(is.na(GID_2))
if (nrow(fallback_stat) > 0) {
  fallback_pts <- st_as_sf(
    fallback_stat, coords = c("fallback_lon", "fallback_lat"),
    crs = 4326, remove = FALSE
  )
  city_pts <- bind_rows(matched_pts, fallback_pts)
} else {
  city_pts <- matched_pts
}

# Cap visual size only; raw values remain unchanged in exports and summaries.
size_cap <- quantile(city_pts$annual_total_loss_musd, 0.98, na.rm = TRUE)
city_pts <- city_pts %>%
  mutate(
    dominant_smod = factor(dominant_smod, levels = smod_levels),
    loss_size = pmin(annual_total_loss_musd, size_cap)
  )

write.csv(st_drop_geometry(city_pts), out_city, row.names = FALSE)

world <- ne_countries(scale = "medium", returnclass = "sf")
lon_label <- function(x) paste0(ifelse(x == 0, "0", abs(x)), "°", ifelse(x < 0, "W", ifelse(x > 0, "E", "")))
lat_label <- function(x) paste0(ifelse(x == 0, "0", abs(x)), "°", ifelse(x < 0, "S", ifelse(x > 0, "N", "")))
size_breaks <- c(0.1, 0.5, 2, 9)
size_breaks <- size_breaks[size_breaks <= size_cap]

p_map <- ggplot() +
  geom_sf(data = world, fill = "#F0F1EF", color = "#C9CCCA", linewidth = 0.22) +
  geom_sf(
    data = city_pts,
    aes(size = loss_size, fill = dominant_smod),
    shape = 21, color = "white", stroke = 0.18, alpha = 0.92
  ) +
  scale_fill_manual(values = smod_pal, breaks = smod_levels, drop = FALSE, name = "Dominant spatial type") +
  scale_size_continuous(
    trans = "sqrt", range = c(0.8, 5.0), breaks = size_breaks,
    labels = label_number(accuracy = 0.1), name = "Annual total loss (M USD)"
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 4, color = "white")),
    size = guide_legend(
      order = 2,
      override.aes = list(fill = "#6F9699", color = "white", alpha = 0.95)
    )
  ) +
  coord_sf(expand = FALSE, xlim = c(-150, 150), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = seq(-150, 150, 60), labels = lon_label) +
  scale_y_continuous(breaks = seq(-30, 60, 30), labels = lat_label) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "#777777", linewidth = 0.45),
    axis.title = element_blank(),
    axis.text = element_text(size = 8.5, color = "#4F4F4F"),
    axis.ticks = element_line(color = "#888888", linewidth = 0.3),
    axis.ticks.length = grid::unit(1.5, "mm"),
    legend.position = "inside",
    legend.position.inside = c(0.025, 0.06),
    legend.justification = c(0, 0),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.88), color = NA),
    legend.title = element_text(size = 8.8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.height = grid::unit(3.4, "mm"),
    plot.margin = margin(5, 7, 4, 7)
  )

ggsave(out_map_svg, p_map, width = 12, height = 5.1, device = svglite)
ggsave(out_map_pdf, p_map, width = 12, height = 5.1)

# Composition by spatial setting, using the same records and palette as the map.
share_df <- bind_rows(
  df %>% count(smod_group, name = "value") %>% mutate(indicator = "Plant count"),
  df %>% group_by(smod_group) %>% summarise(value = sum(exposed_pop, na.rm = TRUE), .groups = "drop") %>%
    mutate(indicator = "Exposed population"),
  df %>% group_by(smod_group) %>% summarise(value = sum(annual_loss_usd, na.rm = TRUE), .groups = "drop") %>%
    mutate(indicator = "Annual total loss")
) %>%
  group_by(indicator) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  mutate(
    indicator = factor(indicator, levels = c("Plant count", "Exposed population", "Annual total loss")),
    indicator_label = factor(
      recode(
        as.character(indicator),
        "Plant count" = "Plant count",
        "Exposed population" = "Exposed pop.",
        "Annual total loss" = "Annual loss"
      ),
      levels = c("Plant count", "Exposed pop.", "Annual loss")
    ),
    smod_group = factor(smod_group, levels = smod_levels),
    label_offset = case_when(
      smod_group == "Urban core" ~ 0.030,
      smod_group == "Peri-urban" ~ 0.025,
      smod_group == "Rural" ~ 0.030,
      smod_group == "Remote" & indicator == "Plant count" ~ -0.025,
      TRUE ~ -0.005
    ),
    label_y = share + label_offset,
    stack_label_color = ifelse(smod_group %in% c("Urban core", "Peri-urban"), "white", "#3F3F3F")
  )

write.csv(share_df, out_summary, row.names = FALSE)

# Clean horizontal 100% stacked bars.
p_stack <- ggplot(share_df, aes(indicator_label, share, fill = smod_group)) +
  geom_col(width = 0.58, color = "white", linewidth = 0.45) +
  geom_text(
    aes(
      label = ifelse(share >= 0.08, percent(share, accuracy = 1), ""),
      color = stack_label_color,
      group = smod_group
    ),
    position = position_stack(vjust = 0.5), size = 3.1,
    fontface = "bold", show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = smod_pal, breaks = smod_levels, drop = FALSE, name = NULL) +
  scale_color_identity() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(x = NULL, y = "Share") +
  theme_classic(base_size = 11) +
  theme(
    panel.border = element_rect(fill = NA, color = "#555555", linewidth = 0.4),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(color = "#3F3F3F"),
    legend.position = "none",
    plot.margin = margin(5, 8, 5, 5)
  )

ggsave(out_stack_svg, p_stack, width = 5.29, height = 3.8, device = svglite)
ggsave(out_stack_pdf, p_stack, width = 5.29, height = 3.8)

cat("Plants used:", nrow(df), "\n")
cat("Admin2 map points:", nrow(city_pts), "\n")
cat("Size cap (P98, M USD):", round(size_cap, 2), "\n\n")
print(share_df %>% select(indicator, smod_group, share) %>% arrange(indicator, smod_group))
cat("\nOutputs:\n", out_map_svg, "\n", out_stack_svg, "\n", sep = "")

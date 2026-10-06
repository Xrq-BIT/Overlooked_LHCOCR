suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(forcats)
  library(scales)
  library(cowplot)
  library(svglite)
})

excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/0706专注煤电与微气候反事实/coal_plants_with_LSM_10km_productivity副本2.xlsx"
out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图二经济损失/图二整体润色版素材"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_svg <- file.path(out_dir, "Fig2c_city_country_lollipop_rankings.svg")
out_pdf <- file.path(out_dir, "Fig2c_city_country_lollipop_rankings.pdf")
out_city_svg <- file.path(out_dir, "Fig2d_city_lollipop_ranking.svg")
out_country_svg <- file.path(out_dir, "Fig2e_country_lollipop_ranking.svg")
out_city_csv <- file.path(out_dir, "Fig2c_top10_cities.csv")
out_country_csv <- file.path(out_dir, "Fig2c_top10_countries.csv")

raw <- read_excel(excel_path)
needed <- c("All_loss_lifecycle_60yr_LSM_10km", "gadm_admin2", "gadm_iso3")
if (!all(needed %in% names(raw))) {
  stop("Missing columns: ", paste(setdiff(needed, names(raw)), collapse = ", "))
}

df <- raw %>%
  transmute(
    loss_usd = suppressWarnings(as.numeric(All_loss_lifecycle_60yr_LSM_10km)),
    admin2 = str_squish(as.character(gadm_admin2)),
    iso3 = str_to_upper(str_trim(as.character(gadm_iso3)))
  ) %>%
  mutate(
    admin2 = ifelse(is.na(admin2) | admin2 == "" | str_to_lower(admin2) %in% c("unknown", "na", "n/a", "none", "null"), NA, admin2),
    iso3 = ifelse(is.na(iso3) | iso3 == "" | str_to_lower(iso3) %in% c("unknown", "na", "n/a", "none", "null"), NA, iso3),
    iso3 = ifelse(iso3 %in% c("TWN", "TWM", "HKG", "MAC"), "CHN", iso3)
  ) %>%
  filter(is.finite(loss_usd), loss_usd >= 0, !is.na(iso3))

top_city <- df %>%
  filter(!is.na(admin2)) %>%
  group_by(admin2, iso3) %>%
  summarise(loss_musd = sum(loss_usd) / 1e6, .groups = "drop") %>%
  arrange(desc(loss_musd)) %>%
  slice_head(n = 10) %>%
  mutate(
    # Display-only names: keep the original GADM admin2 key for aggregation.
    display_admin2 = recode(
      admin2,
      "Muang Rayong" = "Rayong",
      .default = admin2
    ),
    label = paste0(display_admin2, ",", iso3),
    label = fct_reorder(label, loss_musd)
  )

top_country <- df %>%
  group_by(iso3) %>%
  summarise(loss_musd = sum(loss_usd) / 1e6, .groups = "drop") %>%
  arrange(desc(loss_musd)) %>%
  slice_head(n = 10) %>%
  mutate(label = fct_reorder(iso3, loss_musd))

global_loss_usd <- sum(df$loss_usd)
city_top10_share <- sum(top_city$loss_musd) * 1e6 / global_loss_usd
country_top10_share <- sum(top_country$loss_musd) * 1e6 / global_loss_usd

write.csv(top_city, out_city_csv, row.names = FALSE)
write.csv(top_country, out_country_csv, row.names = FALSE)

make_lollipop <- function(dat, x_label, point_color, y_text_size = 10.5) {
  ggplot(dat, aes(loss_musd, label)) +
    geom_segment(
      aes(x = 0, xend = loss_musd, yend = label),
      linewidth = 8.0, color = "#D9DDDE", lineend = "round"
    ) +
    geom_point(size = 4.8, color = point_color) +
    geom_text(
      aes(label = label_number(big.mark = ",", accuracy = 1)(loss_musd)),
      hjust = -0.12, size = 4.4, color = "#4A4A4A"
    ) +
    scale_x_continuous(
      labels = label_number(big.mark = ",", accuracy = 1),
      expand = expansion(mult = c(0, 0.24))
    ) +
    labs(title = NULL, x = x_label, y = NULL) +
    theme_classic(base_size = 14) +
    theme(
      panel.border = element_rect(fill = NA, color = "#555555", linewidth = 0.4),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "#777777", linewidth = 0.3),
      axis.text.y = element_text(size = y_text_size, color = "#3E3E3E", hjust = 1),
      axis.text.x = element_text(size = 12.6, color = "#4A4A4A"),
      axis.title.x = element_text(size = 16.5),
      plot.margin = margin(5, 20, 5, 5)
    )
}

sector_polygon <- function(start, end, radius = 1, dx = 0, dy = 0, n = 240) {
  theta <- seq(start, end, length.out = n)
  data.frame(
    x = c(dx, dx + radius * cos(theta), dx),
    y = c(dy, dy + radius * sin(theta), dy)
  )
}

annular_sector <- function(start, end, outer = 1, inner = 0.57, dx = 0, dy = 0, n = 240) {
  theta_outer <- seq(start, end, length.out = n)
  theta_inner <- rev(theta_outer)
  data.frame(
    x = c(dx + outer * cos(theta_outer), dx + inner * cos(theta_inner)),
    y = c(dy + outer * sin(theta_outer), dy + inner * sin(theta_inner))
  )
}

make_share_pie <- function(share, accent, accent_dark, top_label) {
  other_angle <- 2 * pi * (1 - share)
  # Keep the residual wedge at the bottom and lift the highlighted Top-10 wedge.
  other_start <- 3 * pi / 2 - other_angle / 2
  other_end <- 3 * pi / 2 + other_angle / 2
  top_start <- other_end
  top_end <- other_start + 2 * pi
  lift_y <- 0.08
  depth_y <- -0.08

  other_face <- annular_sector(other_start, other_end)
  top_face <- annular_sector(top_start, top_end, dy = lift_y)
  other_depth <- bind_rows(lapply(seq(-0.06, 0, length.out = 8), function(offset) {
    transform(annular_sector(other_start, other_end, dy = offset), layer = offset)
  }))
  top_depth <- bind_rows(lapply(seq(depth_y, lift_y, length.out = 18), function(offset) {
    transform(annular_sector(top_start, top_end, dy = offset), layer = offset)
  }))

  p <- ggplot() +
    geom_polygon(
      data = other_depth, aes(x, y, group = layer),
      fill = "#BCC2C4", color = NA
    ) +
    geom_polygon(
      data = top_depth, aes(x, y, group = layer),
      fill = accent_dark, color = NA
    ) +
    geom_polygon(data = other_face, aes(x, y), fill = "#E4E7E8", color = "white", linewidth = 0.35) +
    geom_polygon(data = top_face, aes(x, y), fill = accent, color = accent_dark, linewidth = 0.65) +
    annotate(
      "text", x = 0, y = 0.03,
      label = sprintf("%s\n%.1f%%", top_label, 100 * share),
      color = accent_dark, fontface = "bold", size = 3.4, lineheight = 0.82
    ) +
    coord_equal(xlim = c(-1.10, 1.10), ylim = c(-1.12, 1.24), clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  p
}

p_city_base <- make_lollipop(
  top_city, "City-level lifecycle loss (M USD)", "#315B73", y_text_size = 9.8
)
p_country_base <- make_lollipop(
  top_country, "Country-level lifecycle loss (M USD)", "#C97A62"
)

pie_city <- make_share_pie(
  city_top10_share, "#315B73", "#244454", "Top 10\ncities"
)
pie_country <- make_share_pie(
  country_top10_share, "#C97A62", "#995A47", "Top 10\ncountries"
)

# Same normalized inset location in both ranking panels; lower-right space is data-free.
p_city <- p_city_base + annotation_custom(
  ggplotGrob(pie_city),
  xmin = max(top_city$loss_musd) * 0.55,
  xmax = max(top_city$loss_musd) * 1.20,
  ymin = 0.95, ymax = 5.70
)
p_country <- p_country_base + annotation_custom(
  ggplotGrob(pie_country),
  xmin = max(top_country$loss_musd) * 0.55,
  xmax = max(top_country$loss_musd) * 1.20,
  ymin = 0.95, ymax = 5.70
)

combined <- plot_grid(
  p_city, p_country,
  nrow = 1, rel_widths = c(1.10, 0.90), align = "h", axis = "tb"
)

ggsave(out_svg, combined, width = 14.0, height = 3.8, device = svglite)
ggsave(out_pdf, combined, width = 14.0, height = 3.8)
ggsave(out_city_svg, p_city, width = 8.079, height = 5.357, device = svglite)
ggsave(out_country_svg, p_country, width = 6.87, height = 5.357, device = svglite)
rsvg::rsvg_pdf(out_city_svg, sub("\\.svg$", ".pdf", out_city_svg))
rsvg::rsvg_pdf(out_country_svg, sub("\\.svg$", ".pdf", out_country_svg))

cat("Outputs:\n", out_svg, "\n", out_pdf, "\n",
    out_city_svg, "\n", out_country_svg, "\n", sep = "")
cat(sprintf(
  "Top-10 shares of global lifecycle loss: cities %.1f%%; countries %.1f%%\n",
  100 * city_top10_share, 100 * country_top10_share
))

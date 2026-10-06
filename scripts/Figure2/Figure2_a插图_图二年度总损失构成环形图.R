suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(svglite)
})

excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/0706专注煤电与微气候反事实/coal_plants_with_LSM_10km_productivity副本2.xlsx"
out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图二经济损失/图二整体润色版素材"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_svg <- file.path(out_dir, "Fig2a_annual_loss_composition_donut.svg")
out_pdf <- file.path(out_dir, "Fig2a_annual_loss_composition_donut.pdf")

raw <- read_excel(excel_path, sheet = "coal")
needed <- c(
  "Productivity_loss_annual_10km_lsm_worldpop2020",
  "VSL_loss_annual_mean_2011_2020_LSM_all_years"
)
if (!all(needed %in% names(raw))) {
  stop("Missing columns: ", paste(setdiff(needed, names(raw)), collapse = ", "))
}

productivity_loss <- sum(
  suppressWarnings(as.numeric(raw[[needed[1]]])), na.rm = TRUE
)
health_loss <- sum(
  suppressWarnings(as.numeric(raw[[needed[2]]])), na.rm = TRUE
)
total_loss <- productivity_loss + health_loss
productivity_share <- productivity_loss / total_loss
health_share <- health_loss / total_loss

annular_sector <- function(start, end, outer = 0.94, inner = 0.52, n = 360) {
  theta_outer <- seq(start, end, length.out = n)
  theta_inner <- rev(theta_outer)
  data.frame(
    x = c(outer * cos(theta_outer), inner * cos(theta_inner)),
    y = c(outer * sin(theta_outer), inner * sin(theta_inner))
  )
}

start_angle <- pi / 2
productivity_end <- start_angle - 2 * pi * productivity_share
health_end <- start_angle - 2 * pi

productivity_poly <- transform(
  annular_sector(start_angle, productivity_end), component = "Productivity"
)
health_poly <- transform(
  annular_sector(productivity_end, health_end), component = "Health"
)
pie_data <- bind_rows(productivity_poly, health_poly)

label_data <- data.frame(
  component = c("Productivity", "Health"),
  angle = c(
    (start_angle + productivity_end) / 2,
    (productivity_end + health_end) / 2
  ),
  label = c(
    sprintf("Productivity\n%.1f%%", 100 * productivity_share),
    sprintf("Health\n%.1f%%", 100 * health_share)
  )
) %>%
  mutate(
    x0 = 0.88 * cos(angle),
    y0 = 0.88 * sin(angle),
    side = ifelse(cos(angle) >= 0, 1, -1),
    x1 = 1.16 * side,
    y1 = y0,
    x_text = 1.24 * side,
    hjust = ifelse(side > 0, 0, 1),
    text_color = ifelse(component == "Productivity", "#244454", "#995A47")
  )

p <- ggplot(pie_data, aes(x, y, group = component, fill = component)) +
  geom_polygon(color = "white", linewidth = 0.65) +
  geom_segment(
    data = label_data,
    aes(x = x0, y = y0, xend = x1, yend = y1, color = component),
    inherit.aes = FALSE,
    linewidth = 0.45
  ) +
  geom_text(
    data = label_data,
    aes(x = x_text, y = y1, label = label, hjust = hjust, color = component),
    inherit.aes = FALSE,
    fontface = "bold", size = 3.40, lineheight = 0.86
  ) +
  annotate(
    "text", x = 0, y = 0.13,
    label = sprintf("%.2f", total_loss / 1e9),
    color = "#30383D", fontface = "bold", size = 5.5
  ) +
  annotate(
    "text", x = 0, y = -0.08,
    label = "'B USD'~yr^{-1}", parse = TRUE,
    color = "#30383D", size = 3.25
  ) +
  scale_fill_manual(values = c(Productivity = "#315B73", Health = "#C97A62")) +
  scale_color_manual(values = c(Productivity = "#244454", Health = "#995A47")) +
  coord_equal(xlim = c(-2.10, 2.10), ylim = c(-1.12, 1.12), clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

ggsave(out_svg, p, width = 4.4, height = 2.6, device = svglite, bg = "transparent")
ggsave(out_pdf, p, width = 4.4, height = 2.6, bg = "transparent")

cat(sprintf(
  "Annual loss composition: total %.3f B USD; productivity %.1f%%; health %.1f%%\n",
  total_loss / 1e9, 100 * productivity_share, 100 * health_share
))
cat("Saved:\n", out_svg, "\n", out_pdf, "\n", sep = "")

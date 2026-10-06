suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(svglite)
})

base_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图二经济损失"
input_csv <- file.path(
  base_dir, "图二前半部分润色版", "Fig2_smod_share_summary.csv"
)
out_dir <- file.path(base_dir, "Fig2_portrait_middle_sources")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

smod_levels <- c("Urban core", "Peri-urban", "Rural", "Remote")
smod_pal <- c(
  "Urban core" = "#315B73",
  "Peri-urban" = "#6F9699",
  "Rural" = "#C2A56B",
  "Remote" = "#AAA6A0"
)

share_df <- read_csv(input_csv, show_col_types = FALSE) %>%
  mutate(
    indicator_label = factor(
      indicator_label,
      levels = c("Plant count", "Exposed pop.", "Annual loss")
    ),
    smod_group = factor(smod_group, levels = smod_levels),
    label_color = if_else(
      smod_group %in% c("Urban core", "Peri-urban"), "white", "#3F3F3F"
    )
  )

p <- ggplot(share_df, aes(indicator_label, share, fill = smod_group)) +
  geom_col(width = 0.58, color = "white", linewidth = 0.45) +
  geom_text(
    aes(
      label = ifelse(share >= 0.08, percent(share, accuracy = 1), ""),
      color = label_color,
      group = smod_group
    ),
    position = position_stack(vjust = 0.5),
    size = 3.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = smod_pal, breaks = smod_levels, drop = FALSE) +
  scale_color_identity() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = "Share") +
  theme_classic(base_size = 11) +
  theme(
    panel.border = element_rect(fill = NA, color = "#555555", linewidth = 0.4),
    axis.line = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 9.5, color = "#3F3F3F"),
    axis.text.y = element_text(size = 10, color = "#3F3F3F"),
    axis.title.y = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(6, 6, 6, 6)
  )

svg_path <- file.path(out_dir, "Fig2b_smod_share_stacked_portrait.svg")
pdf_path <- file.path(out_dir, "Fig2b_smod_share_stacked_portrait.pdf")
png_path <- file.path(out_dir, "Fig2b_smod_share_stacked_portrait.png")

ggsave(svg_path, p, width = 4.0, height = 5.2, device = svglite::svglite)
rsvg::rsvg_pdf(svg_path, pdf_path)
rsvg::rsvg_png(svg_path, png_path, width = 1200)

cat("Saved:\n", svg_path, "\n", pdf_path, "\n", png_path, "\n", sep = "")

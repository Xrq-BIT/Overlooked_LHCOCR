# Coal-only fuel subtype boxplot using the current analysis workbook.
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(stringr)
  library(rstatix)
  library(ggpubr)
})

excel_path <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/",
  "0706专注煤电与微气候反事实/",
  "coal_plants_with_LSM_10km_productivity副本2.xlsx"
)
out_dir <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/",
  "绘图/图一温差统计/coal_fuel_subtype_boxplot_sig"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

coal_order <- c("Bituminous", "Subbituminous", "Lignite", "Other coal")
coal_pal <- c(
  "Bituminous" = "#8DA7B2",
  "Subbituminous" = "#B7C7CD",
  "Lignite" = "#C97A62",
  "Other coal" = "#B8A78D"
)
winsor_lo <- 0.02
winsor_hi <- 0.98
# The figure reports unadjusted pairwise Wilcoxon p-values and hides p >= 0.05.
use_adjusted_p_for_figure <- FALSE

raw <- read_excel(excel_path)
required_cols <- c("Type", "Fuel", "delt_warm_min")
if (!all(required_cols %in% names(raw))) {
  stop("Missing required columns: ", paste(setdiff(required_cols, names(raw)), collapse = ", "))
}

df <- raw %>%
  transmute(
    Type = str_to_lower(str_trim(as.character(Type))),
    Fuel = as.character(Fuel),
    dT = suppressWarnings(as.numeric(delt_warm_min))
  ) %>%
  filter(Type == "coal", !is.na(Fuel), !is.na(dT)) %>%
  mutate(
    Fuel_lower = str_to_lower(str_trim(Fuel)),
    Fuel_group = case_when(
      str_detect(Fuel_lower, "subbituminous") ~ "Subbituminous",
      str_detect(Fuel_lower, "bituminous") ~ "Bituminous",
      str_detect(Fuel_lower, "lignite") ~ "Lignite",
      TRUE ~ "Other coal"
    ),
    Fuel_group = factor(Fuel_group, levels = coal_order)
  )

if (nrow(df) == 0L) stop("No valid coal observations were found.")

q_lo <- as.numeric(quantile(df$dT, winsor_lo, na.rm = TRUE))
q_hi <- as.numeric(quantile(df$dT, winsor_hi, na.rm = TRUE))
df <- df %>% mutate(dT_plot = pmin(pmax(dT, q_lo), q_hi))

counts <- df %>%
  group_by(Fuel_group, .drop = FALSE) %>%
  summarise(
    n = n(),
    median_dT = median(dT, na.rm = TRUE),
    mean_dT = mean(dT, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(counts, file.path(out_dir, "coal_fuel_group_counts.csv"), row.names = FALSE)

tests <- df %>%
  pairwise_wilcox_test(dT ~ Fuel_group, p.adjust.method = "BH") %>%
  mutate(
    p_figure = if (use_adjusted_p_for_figure) p.adj else p,
    p_label = case_when(
      p_figure < 0.001 ~ "***",
      p_figure < 0.01 ~ "**",
      p_figure < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )
write.csv(tests, file.path(out_dir, "coal_fuel_group_significance_results.csv"), row.names = FALSE)

y_range <- q_hi - q_lo
if (!is.finite(y_range) || y_range <= 0) y_range <- 1
sig_plot <- tests %>%
  filter(p_figure < 0.05) %>%
  arrange(group1, group2) %>%
  mutate(y.position = q_hi + seq(0.08, 0.08 + 0.07 * pmax(n() - 1, 0), length.out = n()) * y_range)

y_top <- if (nrow(sig_plot) > 0L) {
  max(sig_plot$y.position) + 0.08 * y_range
} else {
  q_hi + 0.08 * y_range
}

p <- ggplot(df, aes(Fuel_group, dT_plot, fill = Fuel_group)) +
  geom_point(
    position = position_jitter(width = 0.18, height = 0, seed = 123),
    size = 0.7, alpha = 0.10, color = "#333333"
  ) +
  geom_boxplot(
    width = 0.42, outlier.shape = NA, color = "#303030",
    alpha = 0.88, linewidth = 0.62, show.legend = FALSE
  ) +
  scale_fill_manual(values = coal_pal, drop = FALSE) +
  scale_y_continuous(
    limits = c(q_lo, y_top),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = NULL, y = expression(Delta*T), title = NULL) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "#4C4C4C", linewidth = 0.45),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "#666666", linewidth = 0.35),
    plot.title = element_blank(),
    axis.text.x = element_text(size = 10.5, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11, color = "#3F3F3F"),
    axis.title.y = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(6, 6, 6, 6)
  )

if (nrow(sig_plot) > 0L) {
  p <- p + stat_pvalue_manual(
    sig_plot,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.45,
    size = 3.5,
    hide.ns = TRUE
  )
}

ggsave(
  file.path(out_dir, "coal_fuel_subtype_boxplot_sig_unadjusted_p.pdf"),
  p, width = 5.0, height = 4.5, device = grDevices::pdf
)
ggsave(
  file.path(out_dir, "coal_fuel_subtype_boxplot_sig_unadjusted_p.svg"),
  p, width = 5.0, height = 4.5, device = svglite::svglite
)
ggsave(
  file.path(out_dir, "coal_fuel_subtype_boxplot_sig_unadjusted_p.png"),
  p, width = 5.0, height = 4.5, dpi = 300, bg = "white"
)

print(counts)
print(tests)
message("Saved coal-only figure to: ", out_dir)

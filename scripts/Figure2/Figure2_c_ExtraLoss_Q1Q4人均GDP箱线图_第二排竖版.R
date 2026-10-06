suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(svglite)
})

excel_path <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/",
  "0706专注煤电与微气候反事实/",
  "coal_plants_with_LSM_10km_productivity副本2.xlsx"
)
out_dir <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图二经济损失/",
  "Fig2_portrait_middle_sources"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dat <- read_excel(excel_path, sheet = "coal") %>%
  transmute(
    extra_loss = suppressWarnings(as.numeric(extraloss300_10km_lsm)),
    gdp_pc = suppressWarnings(as.numeric(GDP_per_capita_2019))
  ) %>%
  filter(
    is.finite(extra_loss), extra_loss >= 0,
    is.finite(gdp_pc), gdp_pc > 0
  ) %>%
  mutate(extra_quartile = ntile(extra_loss, 4)) %>%
  filter(extra_quartile %in% c(1L, 4L)) %>%
  mutate(
    group = factor(
      if_else(
        extra_quartile == 1L,
        "ExtraLoss Q1 (lowest)",
        "ExtraLoss Q4 (highest)"
      ),
      levels = c("ExtraLoss Q1 (lowest)", "ExtraLoss Q4 (highest)")
    )
  )

q1 <- dat %>%
  filter(group == "ExtraLoss Q1 (lowest)") %>%
  pull(gdp_pc)
q4 <- dat %>%
  filter(group == "ExtraLoss Q4 (highest)") %>%
  pull(gdp_pc)

wilcox_result <- wilcox.test(q4, q1, exact = FALSE)
welch_result <- t.test(q4, q1, var.equal = FALSE)

significance <- case_when(
  wilcox_result$p.value < 0.001 ~ "***",
  wilcox_result$p.value < 0.01 ~ "**",
  wilcox_result$p.value < 0.05 ~ "*",
  TRUE ~ "ns"
)

test_table <- data.frame(
  comparison = "ExtraLoss Q4 vs Q1",
  q1_n = length(q1),
  q4_n = length(q4),
  q1_mean_gdp_per_capita = mean(q1),
  q4_mean_gdp_per_capita = mean(q4),
  q1_median_gdp_per_capita = median(q1),
  q4_median_gdp_per_capita = median(q4),
  wilcoxon_p = wilcox_result$p.value,
  welch_p = welch_result$p.value,
  significance = significance
)
write.csv(
  test_table,
  file.path(out_dir, "ExtraLoss_Q1_Q4_GDP_significance.csv"),
  row.names = FALSE
)

# Limit only the displayed range; all raw observations enter both tests above.
display_lower <- quantile(dat$gdp_pc, 0.05, na.rm = TRUE)
display_upper <- quantile(dat$gdp_pc, 0.95, na.rm = TRUE)
plot_dat <- dat %>%
  mutate(gdp_plot = pmin(pmax(gdp_pc, display_lower), display_upper))

y_min <- min(plot_dat$gdp_plot)
y_max <- max(plot_dat$gdp_plot)
y_range <- y_max - y_min
y_sig <- y_max + 0.045 * y_range
y_limit <- y_max + 0.105 * y_range

group_pal <- c(
  "ExtraLoss Q1 (lowest)" = "#C9DCE6",
  "ExtraLoss Q4 (highest)" = "#C97A62"
)

p <- ggplot(plot_dat, aes(group, gdp_plot, fill = group)) +
  geom_point(
    position = position_jitter(width = 0.16, height = 0, seed = 123),
    size = 0.65, alpha = 0.10, color = "#333333", show.legend = FALSE
  ) +
  geom_boxplot(
    width = 0.46, outlier.shape = NA, color = "#303030", alpha = 0.88,
    linewidth = 0.62, show.legend = FALSE
  ) +
  annotate(
    "segment", x = 1, xend = 2, y = y_sig, yend = y_sig,
    linewidth = 0.55
  ) +
  annotate(
    "segment", x = 1, xend = 1,
    y = y_sig - 0.020 * y_range, yend = y_sig, linewidth = 0.55
  ) +
  annotate(
    "segment", x = 2, xend = 2,
    y = y_sig - 0.020 * y_range, yend = y_sig, linewidth = 0.55
  ) +
  annotate(
    "text", x = 1.5, y = y_sig + 0.022 * y_range,
    label = significance, size = 4, fontface = "bold"
  ) +
  scale_fill_manual(values = group_pal, drop = FALSE) +
  scale_x_discrete(
    labels = c(
      "ExtraLoss Q1 (lowest)" = "ExtraLoss Q1 (low)",
      "ExtraLoss Q4 (highest)" = "ExtraLoss Q4 (high)"
    ),
    expand = expansion(add = 0.4)
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ",", accuracy = 1),
    limits = c(y_min, y_limit),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "GDP per capita ($)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA, color = "#4C4C4C", linewidth = 0.45
    ),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "#666666", linewidth = 0.35),
    axis.text.x = element_text(size = 9.5, color = "#3F3F3F"),
    axis.text.y = element_text(size = 10, color = "#3F3F3F"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(6, 6, 6, 6)
  )

svg_path <- file.path(out_dir, "ExtraLoss_Q1_Q4_GDP_boxplot.svg")
pdf_path <- file.path(out_dir, "ExtraLoss_Q1_Q4_GDP_boxplot.pdf")
png_path <- file.path(out_dir, "ExtraLoss_Q1_Q4_GDP_boxplot.png")

ggsave(svg_path, p, width = 4.0, height = 5.2, device = svglite::svglite)
rsvg::rsvg_pdf(svg_path, pdf_path)
rsvg::rsvg_png(svg_path, png_path, width = 1200)

print(test_table)
cat("Saved:\n", svg_path, "\n", pdf_path, "\n", png_path, "\n", sep = "")

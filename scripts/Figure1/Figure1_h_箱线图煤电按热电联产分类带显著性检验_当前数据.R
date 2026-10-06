# Coal-only CHP comparison using the current analysis workbook.
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(ggpubr)
})

excel_path <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/",
  "0706专注煤电与微气候反事实/",
  "coal_plants_with_LSM_10km_productivity副本2.xlsx"
)
out_dir <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/",
  "绘图/图一温差统计/coal_chp_significance_boxplot"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

raw <- read_excel(excel_path)
required_cols <- c("Type", "CHP", "delt_warm_min")
if (!all(required_cols %in% names(raw))) {
  stop("Missing required columns: ", paste(setdiff(required_cols, names(raw)), collapse = ", "))
}

df <- raw %>%
  transmute(
    Type = tolower(trimws(as.character(Type))),
    CHP = tolower(trimws(as.character(CHP))),
    dT = suppressWarnings(as.numeric(delt_warm_min))
  ) %>%
  filter(Type == "coal", CHP %in% c("yes", "no"), !is.na(dT)) %>%
  mutate(CHP = factor(CHP, levels = c("yes", "no"), labels = c("CHP_Yes", "CHP_No")))

if (n_distinct(df$CHP) != 2L) stop("Both CHP groups are required for the test.")

q_lo <- as.numeric(quantile(df$dT, 0.01, na.rm = TRUE))
q_hi <- as.numeric(quantile(df$dT, 0.99, na.rm = TRUE))
df <- df %>% mutate(dT_plot = pmin(pmax(dT, q_lo), q_hi))

test <- wilcox.test(dT ~ CHP, data = df, exact = FALSE)
p_value <- unname(test$p.value)
p_label <- case_when(
  p_value < 0.001 ~ "***",
  p_value < 0.01 ~ "**",
  p_value < 0.05 ~ "*",
  TRUE ~ "ns"
)

result <- df %>%
  group_by(CHP) %>%
  summarise(
    n = n(),
    mean_dT = mean(dT),
    median_dT = median(dT),
    .groups = "drop"
  ) %>%
  mutate(p_value = p_value, p_label = p_label)
write.csv(result, file.path(out_dir, "coal_chp_wilcox_results.csv"), row.names = FALSE)

y_range <- q_hi - q_lo
if (!is.finite(y_range) || y_range <= 0) y_range <- 1
annotation <- data.frame(
  group1 = "CHP_Yes",
  group2 = "CHP_No",
  y.position = q_hi + 0.08 * y_range,
  p_label = p_label
)

p <- ggplot(df, aes(CHP, dT_plot, fill = CHP)) +
  geom_point(
    position = position_jitter(width = 0.18, height = 0, seed = 123),
    size = 0.7, alpha = 0.10, color = "#333333", show.legend = FALSE
  ) +
  geom_boxplot(
    width = 0.42, outlier.shape = NA, color = "#303030",
    alpha = 0.88, linewidth = 0.62, show.legend = FALSE
  ) +
  stat_pvalue_manual(
    annotation,
    label = "p_label",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = 0.02,
    bracket.size = 0.6,
    size = 5
  ) +
  scale_fill_manual(values = c("CHP_Yes" = "#C97A62", "CHP_No" = "#8DA7B2")) +
  scale_y_continuous(
    limits = c(q_lo, q_hi + 0.18 * y_range),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = NULL, y = expression(Delta*T), title = NULL) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "#4C4C4C", linewidth = 0.45),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "#666666", linewidth = 0.35),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 11, color = "#3F3F3F"),
    axis.title.y = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(6, 6, 6, 6)
  )

ggsave(
  file.path(out_dir, "coal_chp_yes_no_wilcox_boxplot_sig.pdf"),
  p, width = 5.0, height = 4.5, device = grDevices::pdf
)
ggsave(
  file.path(out_dir, "coal_chp_yes_no_wilcox_boxplot_sig.svg"),
  p, width = 5.0, height = 4.5, device = svglite::svglite
)
ggsave(
  file.path(out_dir, "coal_chp_yes_no_wilcox_boxplot_sig.png"),
  p, width = 5.0, height = 4.5, dpi = 300, bg = "white"
)

print(result)
message("Saved coal-only CHP figure to: ", out_dir)

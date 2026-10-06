# =========================
# 0) Packages
# =========================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(stringr)
  library(forcats)
  library(scales)
  library(ggrepel)
  library(cowplot)
  library(rsvg)
})

# =========================
# 1) File paths
# =========================
excel_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/未来情景/coal_plant_future_result2.xlsx"

out_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图三退役与未来情景/city_persistent_risk_quadrant_gadm_admin2_top10_each_nature_musd"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# =========================
# 2) Scenario config
# =========================
args <- commandArgs(trailingOnly = TRUE)
SCENARIO <- if (length(args) >= 1) str_to_upper(args[1]) else "SSP126"
if (!SCENARIO %in% c("SSP126", "SSP245", "SSP370")) {
  stop("情景必须是 SSP126、SSP245 或 SSP370")
}
YEAR_USE <- 2050

BASE_LOSS_COL   <- "All_loss_annual_mean_LSM_10km"
FUTURE_LOSS_COL <- paste0("loss_", SCENARIO, "_", YEAR_USE)
INCLUDE_COL     <- "future_loss_analysis_included_2021"

# 直接使用 Excel 中已经匹配好的 GADM 城市级行政单元
CITY_COL <- "gadm_admin2"
ISO3_COL <- "gadm_iso3"

POP_COL   <- "pop_10km_total_worldpop2020"
CHILD_COL <- "ratio_10km_child_worldpop2020"
OLD_COL   <- "ratio_10km_old_worldpop2020"
GDP_COL   <- "GDP_per_capita_2019"

DISPLAY_MAX_CHARS <- 30
TOP_EACH_QUADRANT <- 10
TOP_RANK_N <- 20

# =========================
# 3) Output files
# =========================
OUT_CSV <- file.path(
  out_dir,
  paste0("city_persistent_risk_", SCENARIO, "_", YEAR_USE, "_all.csv")
)

OUT_QUADRANT_TOP_CSV <- file.path(
  out_dir,
  paste0("city_quadrant_top", TOP_EACH_QUADRANT, "_each_", SCENARIO, "_", YEAR_USE, ".csv")
)

OUT_PERSISTENT_CSV <- file.path(
  out_dir,
  paste0("city_persistent_risk_top", TOP_RANK_N, "_", SCENARIO, "_", YEAR_USE, ".csv")
)

OUT_SCATTER_SVG <- file.path(
  out_dir,
  paste0("city_quadrant_top", TOP_EACH_QUADRANT, "_each_", SCENARIO, "_", YEAR_USE, "_scatter.svg")
)

OUT_SCATTER_PDF <- file.path(
  out_dir,
  paste0("city_quadrant_top", TOP_EACH_QUADRANT, "_each_", SCENARIO, "_", YEAR_USE, "_scatter.pdf")
)

# 新增：单独导出象限图图例
OUT_SCATTER_LEGEND_SVG <- file.path(
  out_dir,
  paste0("city_quadrant_top", TOP_EACH_QUADRANT, "_each_", SCENARIO, "_", YEAR_USE, "_legend.svg")
)

OUT_SCATTER_LEGEND_PDF <- file.path(
  out_dir,
  paste0("city_quadrant_top", TOP_EACH_QUADRANT, "_each_", SCENARIO, "_", YEAR_USE, "_legend.pdf")
)

OUT_RANK_SVG <- file.path(
  out_dir,
  paste0("city_persistent_risk_top", TOP_RANK_N, "_", SCENARIO, "_", YEAR_USE, ".svg")
)

OUT_RANK_PDF <- file.path(
  out_dir,
  paste0("city_persistent_risk_top", TOP_RANK_N, "_", SCENARIO, "_", YEAR_USE, ".pdf")
)

# =========================
# 4) Read plant data
# =========================
df_raw <- read_excel(excel_path)

need_cols <- c(
  CITY_COL, ISO3_COL,
  BASE_LOSS_COL, FUTURE_LOSS_COL, INCLUDE_COL
)

if (!all(need_cols %in% names(df_raw))) {
  stop("缺少必要列：", paste(setdiff(need_cols, names(df_raw)), collapse = ", "))
}

has_pop   <- POP_COL %in% names(df_raw)
has_child <- CHILD_COL %in% names(df_raw)
has_old   <- OLD_COL %in% names(df_raw)
has_gdp   <- GDP_COL %in% names(df_raw)

plants_df <- df_raw %>%
  mutate(
    city_raw = as.character(.data[[CITY_COL]]),
    iso3_raw = as.character(.data[[ISO3_COL]]),
    included = suppressWarnings(as.numeric(.data[[INCLUDE_COL]])),
    
    baseline_loss_usd = suppressWarnings(as.numeric(.data[[BASE_LOSS_COL]])),
    future_loss_usd   = suppressWarnings(as.numeric(.data[[FUTURE_LOSS_COL]])),
    
    pop = if (has_pop) {
      suppressWarnings(as.numeric(.data[[POP_COL]]))
    } else {
      NA_real_
    },
    
    child_ratio = if (has_child) {
      suppressWarnings(as.numeric(.data[[CHILD_COL]]))
    } else {
      NA_real_
    },
    
    old_ratio = if (has_old) {
      suppressWarnings(as.numeric(.data[[OLD_COL]]))
    } else {
      NA_real_
    },
    
    gdp_pc = if (has_gdp) {
      suppressWarnings(as.numeric(.data[[GDP_COL]]))
    } else {
      NA_real_
    }
  ) %>%
  transmute(
    city_raw = str_trim(city_raw),
    iso3_raw = str_to_upper(str_trim(iso3_raw)),
    included,
    
    baseline_loss_usd,
    future_loss_usd,
    mitigation_loss_usd = baseline_loss_usd - future_loss_usd,
    
    pop,
    child_ratio,
    old_ratio,
    vulnerable_ratio = child_ratio + old_ratio,
    gdp_pc
  ) %>%
  filter(
    included == 1,
    !is.na(city_raw),
    city_raw != "",
    !is.na(iso3_raw),
    iso3_raw != "",
    !is.na(baseline_loss_usd),
    !is.na(future_loss_usd),
    !is.na(mitigation_loss_usd)
  ) %>%
  mutate(
    mitigation_loss_usd = pmax(mitigation_loss_usd, 0),
    
    city_raw = ifelse(
      str_to_lower(city_raw) %in% c("unknown", "na", "n/a", "none", "null"),
      NA_character_,
      city_raw
    ),
    
    iso3_raw = ifelse(
      str_to_lower(iso3_raw) %in% c("unknown", "na", "n/a", "none", "null"),
      NA_character_,
      iso3_raw
    ),
    
    # 将台湾、香港、澳门并入 CHN
    iso3_raw = case_when(
      iso3_raw %in% c("TWN", "HKG", "MAC") ~ "CHN",
      TRUE ~ iso3_raw
    )
  ) %>%
  filter(
    !is.na(city_raw),
    !is.na(iso3_raw)
  )

cat("有效电厂数：", nrow(plants_df), "\n")
cat("使用基础损失列：", BASE_LOSS_COL, "\n")
cat("使用未来损失列：", FUTURE_LOSS_COL, "\n")

# =========================
# 5) City-level aggregation
# =========================
city_df <- plants_df %>%
  group_by(city_raw, iso3_raw) %>%
  summarise(
    plant_count = n(),
    
    baseline_loss_usd = sum(baseline_loss_usd, na.rm = TRUE),
    future_loss_usd   = sum(future_loss_usd, na.rm = TRUE),
    mitigation_loss_usd = sum(mitigation_loss_usd, na.rm = TRUE),
    
    baseline_loss_musd = baseline_loss_usd / 1e6,
    future_loss_musd   = future_loss_usd / 1e6,
    mitigation_loss_musd = mitigation_loss_usd / 1e6,
    
    reduction_rate = ifelse(
      baseline_loss_usd > 0,
      mitigation_loss_usd / baseline_loss_usd,
      NA_real_
    ),
    
    pop_sum = if (has_pop) {
      sum(pop, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    city_vulnerable_share = if (has_pop & has_child & has_old) {
      weighted.mean(vulnerable_ratio, w = pop, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    city_gdp_pc = if (has_pop & has_gdp) {
      weighted.mean(gdp_pc, w = pop, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    city_name = city_raw,
    iso3_final = iso3_raw,
    
    city_name_short = str_trunc(
      city_name,
      width = DISPLAY_MAX_CHARS,
      side = "right"
    ),
    city_name_short = stringi::stri_trans_general(city_name_short, "Latin-ASCII"),
    
    city_label_plot = paste0(city_name_short, "_", iso3_final)
  ) %>%
  filter(
    !is.na(city_label_plot),
    baseline_loss_musd > 0,
    !is.na(future_loss_musd),
    !is.na(mitigation_loss_musd),
    !is.na(reduction_rate)
  )

# =========================
# 6) Define quadrants + persistent risk score
# =========================
eps <- 1e-6

loss_threshold <- quantile(city_df$baseline_loss_musd, 0.80, na.rm = TRUE)

reduction_threshold_raw <- median(city_df$reduction_rate, na.rm = TRUE)
positive_reduction <- city_df$reduction_rate[
  is.finite(city_df$reduction_rate) & city_df$reduction_rate > 0
]
if (!is.finite(reduction_threshold_raw) || reduction_threshold_raw <= 0) {
  reduction_threshold <- if (length(positive_reduction) > 0) {
    median(positive_reduction, na.rm = TRUE)
  } else {
    0.5
  }
} else {
  reduction_threshold <- reduction_threshold_raw
}

city_df <- city_df %>%
  mutate(
    risk_quadrant = case_when(
      baseline_loss_musd >= loss_threshold & reduction_rate <= reduction_threshold ~
        "High loss + low mitigation",
      
      baseline_loss_musd >= loss_threshold & reduction_rate > reduction_threshold ~
        "High loss + high mitigation",
      
      baseline_loss_musd < loss_threshold & reduction_rate > reduction_threshold ~
        "Low loss + high mitigation",
      
      TRUE ~ "Low loss + low mitigation"
    ),
    
    risk_quadrant = factor(
      risk_quadrant,
      levels = c(
        "High loss + low mitigation",
        "High loss + high mitigation",
        "Low loss + high mitigation",
        "Low loss + low mitigation"
      )
    ),
    
    baseline_loss_pct = percent_rank(baseline_loss_musd),
    future_loss_pct   = percent_rank(future_loss_musd),
    mitigation_pct    = percent_rank(mitigation_loss_musd),
    reduction_pct     = percent_rank(reduction_rate),
    
    persistent_risk_score = baseline_loss_pct + future_loss_pct
  ) %>%
  arrange(desc(persistent_risk_score))

print(table(city_df$risk_quadrant, useNA = "ifany"))
cat("loss_threshold =", loss_threshold, "\n")
cat("reduction_threshold_raw =", reduction_threshold_raw, "\n")
cat("reduction_threshold_used =", reduction_threshold, "\n")

write.csv(city_df, OUT_CSV, row.names = FALSE)

# =========================
# 7) Select Top 10 from each quadrant
# =========================
top_each_quadrant <- city_df %>%
  group_by(risk_quadrant) %>%
  group_modify(~ {
    dat <- .x
    
    q_name <- as.character(.y$risk_quadrant)
    
    if (q_name == "High loss + low mitigation") {
      dat %>% arrange(desc(baseline_loss_musd)) %>% slice_head(n = TOP_EACH_QUADRANT)
      
    } else if (q_name == "High loss + high mitigation") {
      dat %>% arrange(desc(baseline_loss_musd)) %>% slice_head(n = TOP_EACH_QUADRANT)
      
    } else if (q_name == "Low loss + high mitigation") {
      dat %>% arrange(desc(mitigation_loss_musd)) %>% slice_head(n = TOP_EACH_QUADRANT)
      
    } else {
      dat %>% arrange(desc(baseline_loss_musd)) %>% slice_head(n = TOP_EACH_QUADRANT)
    }
  }) %>%
  ungroup() %>%
  mutate(
    label_use = city_label_plot
  )

write.csv(top_each_quadrant, OUT_QUADRANT_TOP_CSV, row.names = FALSE)

cat("\nTop cities in each quadrant:\n")
print(
  top_each_quadrant %>%
    select(
      risk_quadrant,
      city_label_plot,
      plant_count,
      baseline_loss_musd,
      future_loss_musd,
      mitigation_loss_musd,
      reduction_rate,
      persistent_risk_score,
      city_vulnerable_share,
      city_gdp_pc
    ) %>%
    arrange(risk_quadrant, desc(baseline_loss_musd)),
  n = 4 * TOP_EACH_QUADRANT
)

# Plot every city at its observed value. No display-only coordinate spreading.
plot_df <- city_df %>%
  mutate(
    x_plot = baseline_loss_musd,
    y_plot = 100 * reduction_rate
  )

label_df <- bind_rows(
  plot_df %>%
    filter(risk_quadrant == "High loss + low mitigation") %>%
    arrange(desc(baseline_loss_musd)) %>%
    slice_head(n = 6) %>%
    mutate(highlight_group = "Persistent risk"),
  plot_df %>%
    filter(risk_quadrant == "High loss + high mitigation") %>%
    arrange(desc(baseline_loss_musd)) %>%
    slice_head(n = 3) %>%
    mutate(highlight_group = "Strong mitigation")
)

# =========================
# 8) Top persistent risk ranking data
# =========================
top_persistent <- city_df %>%
  filter(!is.na(city_label_plot)) %>%
  arrange(desc(persistent_risk_score)) %>%
  slice_head(n = TOP_RANK_N) %>%
  mutate(
    city_label_short = str_trunc(city_label_plot, width = DISPLAY_MAX_CHARS, side = "right"),
    city_label_short = fct_reorder(city_label_short, future_loss_musd)
  )

write.csv(top_persistent, OUT_PERSISTENT_CSV, row.names = FALSE)

cat("\nTop persistent-risk cities:\n")
print(
  top_persistent %>%
    select(
      city_label_plot,
      plant_count,
      baseline_loss_musd,
      future_loss_musd,
      mitigation_loss_musd,
      reduction_rate,
      persistent_risk_score,
      city_vulnerable_share,
      city_gdp_pc
    ),
  n = TOP_RANK_N
)

# =========================
# 9A) Scatter plot: top cities in each quadrant
# =========================
highlight_pal <- c(
  "Persistent risk" = "#B56F58",
  "Strong mitigation" = "#3E718B"
)

p_scatter <- ggplot(plot_df, aes(x = x_plot, y = y_plot)) +
  geom_vline(
    xintercept = loss_threshold,
    linetype = "dashed",
    linewidth = 0.38,
    color = "#8A8A8A"
  ) +
  geom_hline(
    yintercept = 100 * reduction_threshold,
    linetype = "dashed",
    linewidth = 0.38,
    color = "#8A8A8A"
  ) +
  geom_point(
    color = "#AEB5B4", size = 1.35, alpha = 0.45
  ) +
  geom_point(
    data = label_df,
    aes(fill = highlight_group),
    shape = 21, color = "white", stroke = 0.38,
    size = 3.0, alpha = 0.96
  ) +
  ggrepel::geom_text_repel(
    data = label_df,
    aes(label = city_label_plot),
    size = 2.35,
    color = "#303030",
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.size = 0.20,
    segment.color = "#8A8A8A",
    box.padding = 0.34,
    point.padding = 0.28,
    force = 2.5,
    force_pull = 0.25,
    direction = "both",
    show.legend = FALSE
  ) +
  scale_x_log10(
    labels = label_number(big.mark = ",", accuracy = 0.1),
    expand = expansion(mult = c(0.05, 0.16))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_fill_manual(
    values = highlight_pal,
    name = NULL
  ) +
  labs(
    x = "Baseline annual loss (M USD, log scale)",
    y = "Annual loss reduction by 2050"
  ) +
  theme_classic(base_size = 11, base_family = "sans") +
  theme(
    axis.line = element_line(color = "#555555", linewidth = 0.40),
    axis.text = element_text(size = 9, color = "#444444"),
    axis.title = element_text(size = 10.2, color = "#222222"),
    axis.ticks = element_line(color = "#666666", linewidth = 0.32),
    axis.ticks.length = unit(1.7, "mm"),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.text = element_text(size = 8.2, color = "#333333"),
    legend.key = element_blank(),
    legend.key.width = unit(5, "mm"),
    plot.margin = margin(7, 9, 5, 7)
  )

ggsave(
  OUT_SCATTER_SVG,
  p_scatter,
  width = 8.8,
  height = 6.4,
  device = svglite::svglite
)

rsvg_pdf(OUT_SCATTER_SVG, OUT_SCATTER_PDF)

# =========================
# 9B) Ranking plot: persistent residual-risk cities
# =========================
p_rank <- ggplot(
  top_persistent,
  aes(x = future_loss_musd, y = city_label_short)
) +
  geom_col(
    aes(fill = city_vulnerable_share),
    width = 0.70,
    alpha = 0.92
  ) +
  geom_point(
    aes(size = plant_count),
    color = "grey20",
    fill = "white",
    shape = 21,
    stroke = 0.45,
    alpha = 0.95
  ) +
  geom_text(
    aes(
      label = paste0(
        label_number(big.mark = ",", accuracy = 0.1)(future_loss_musd),
        " M"
      )
    ),
    hjust = -0.08,
    size = 3.0,
    color = "grey10"
  ) +
  scale_x_continuous(
    labels = label_number(big.mark = ",", accuracy = 0.1),
    expand = expansion(mult = c(0, 0.22))
  ) +
  scale_fill_gradientn(
    colours = c("#FEE8C8", "#FDBB84", "#E34A33", "#7F0000"),
    labels = percent_format(accuracy = 1),
    name = "Vulnerable\npopulation share"
  ) +
  scale_size_continuous(
    range = c(2.0, 6.0),
    breaks = c(1, 2, 5, 10),
    name = "Plant count"
  ) +
  labs(
    x = paste0(
      "Future residual annual economic loss under ",
      SCENARIO, "-", YEAR_USE,
      " (M USD)"
    ),
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.75),
    axis.line = element_blank(),
    
    axis.text.y = element_text(size = 8.5, color = "grey20"),
    axis.text.x = element_text(size = 10, color = "grey20"),
    axis.title.x = element_text(size = 11.5, color = "black"),
    axis.ticks = element_line(color = "grey30", linewidth = 0.35),
    axis.ticks.length = unit(2, "mm"),
    
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.03),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "grey20", linewidth = 0.35),
    legend.title = element_text(size = 8.8, color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    legend.key = element_blank(),
    
    plot.margin = margin(6, 28, 6, 6)
  )

ggsave(
  OUT_RANK_SVG,
  p_rank,
  width = 8.8,
  height = 6.4,
  device = svglite::svglite
)

ggsave(
  OUT_RANK_PDF,
  p_rank,
  width = 8.8,
  height = 6.4
)

message(
  "Done!\n",
  "- ", OUT_CSV, "\n",
  "- ", OUT_QUADRANT_TOP_CSV, "\n",
  "- ", OUT_PERSISTENT_CSV, "\n",
  "- ", OUT_SCATTER_SVG, "\n",
  "- ", OUT_SCATTER_LEGEND_SVG, "\n",
  "- ", OUT_RANK_SVG
)

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(svglite)
})

args <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(args) >= 1) args[[1]] else "SSP126"
if (!scenario %in% c("SSP126", "SSP370")) stop("Scenario must be SSP126 or SSP370")
scenario_label <- if (scenario == "SSP126") "SSP1-2.6" else "SSP3-7.0"
selection_mode <- if (length(args) >= 2) args[[2]] else "representative"
if (!selection_mode %in% c("representative", "baseline_top10", "remaining_top10")) {
  stop("Selection mode must be representative, baseline_top10 or remaining_top10")
}

base_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图三退役与未来情景"
source_dir <- file.path(base_dir, "city_persistent_risk_quadrant_gadm_admin2_top10_each_nature_musd")
out_dir <- file.path(
  base_dir,
  if (selection_mode == "representative") {
    "city_loss_mitigation_quadrant_percentile_fig2_palette"
  } else if (selection_mode == "baseline_top10") {
    "city_loss_mitigation_quadrant_baseline_top10_comparison"
  } else {
    "city_loss_mitigation_quadrant_remaining_top10_comparison"
  }
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plot_width <- as.numeric(Sys.getenv("QUADRANT_PLOT_WIDTH", "10.4"))
plot_height <- as.numeric(Sys.getenv("QUADRANT_PLOT_HEIGHT", "6.4"))
plot_suffix <- Sys.getenv("QUADRANT_PLOT_SUFFIX", "")

input_csv <- file.path(source_dir, paste0("city_persistent_risk_", scenario, "_2050_all.csv"))
city <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)
needed <- c("city_label_plot", "plant_count", "baseline_loss_musd", "mitigation_loss_musd", "reduction_rate")
if (!all(needed %in% names(city))) {
  stop("Missing columns: ", paste(setdiff(needed, names(city)), collapse = ", "))
}
city <- city[
  is.finite(city$baseline_loss_musd) & city$baseline_loss_musd > 0 &
    is.finite(city$mitigation_loss_musd) & city$mitigation_loss_musd >= 0,
]
city$remaining_loss_musd <- pmax(
  city$baseline_loss_musd - city$mitigation_loss_musd,
  0
)

# Percentile axes form a relative-priority matrix. Avoided loss is bounded by
# baseline loss, so absolute-value axes otherwise force cities onto a diagonal.
percentile_rank <- function(x) {
  n <- length(x)
  if (n <= 1) return(rep(0.5, n))
  (rank(x, ties.method = "average") - 0.5) / n
}
city$loss_percentile <- percentile_rank(city$baseline_loss_musd)
city$mitigation_percentile <- 0
positive <- city$mitigation_loss_musd > 0
if (any(positive)) {
  city$mitigation_percentile[positive] <- rank(
    city$mitigation_loss_musd[positive], ties.method = "average"
  ) / sum(positive)
}

loss_threshold <- 0.80
mitigation_threshold <- 0.50
city$risk_quadrant <- ifelse(
  city$loss_percentile >= loss_threshold & city$mitigation_percentile < mitigation_threshold,
  "High loss + low mitigation",
  ifelse(
    city$loss_percentile >= loss_threshold & city$mitigation_percentile >= mitigation_threshold,
    "High loss + high mitigation",
    ifelse(
      city$loss_percentile < loss_threshold & city$mitigation_percentile >= mitigation_threshold,
      "Low loss + high mitigation",
      "Low loss + low mitigation"
    )
  )
)
quadrant_order <- c(
  "High loss + low mitigation", "High loss + high mitigation",
  "Low loss + high mitigation", "Low loss + low mitigation"
)
city$risk_quadrant <- factor(city$risk_quadrant, levels = quadrant_order)

# Top10 means closest to the outer corner of the corresponding quadrant.
# Both baseline loss and avoided loss receive equal weight.
city$quadrant_extremeness <- ifelse(
  city$risk_quadrant == "High loss + low mitigation",
  (city$loss_percentile + 1 - city$mitigation_percentile) / 2,
  ifelse(
    city$risk_quadrant == "High loss + high mitigation",
    (city$loss_percentile + city$mitigation_percentile) / 2,
    ifelse(
      city$risk_quadrant == "Low loss + high mitigation",
      (1 - city$loss_percentile + city$mitigation_percentile) / 2,
      (1 - city$loss_percentile + 1 - city$mitigation_percentile) / 2
    )
  )
)

pick_strict_top <- function(df, group_name, n = 10) {
  z <- df[df$risk_quadrant == group_name, , drop = FALSE]
  if (!nrow(z)) return(z)
  z <- z[order(-z$quadrant_extremeness, -z$mitigation_loss_musd, -z$baseline_loss_musd), , drop = FALSE]
  z$quadrant_rank <- seq_len(nrow(z))
  head(z, n)
}
strict_top <- do.call(
  rbind,
  lapply(quadrant_order, function(q) pick_strict_top(city, q, 10))
)

# A strict score Top10 inevitably collapses into the outer corner. For direct
# labels, select 10 non-redundant representatives from the upper half of each
# quadrant's priority-score distribution. The highest-scoring city is retained;
# subsequent cities maximize their minimum 2-D distance from those selected.
pick_representative <- function(df, group_name, n = 10) {
  z <- df[df$risk_quadrant == group_name, , drop = FALSE]
  if (!nrow(z)) return(z)
  z <- z[order(-z$quadrant_extremeness, -z$mitigation_loss_musd, -z$baseline_loss_musd), , drop = FALSE]
  z$strict_quadrant_rank <- seq_len(nrow(z))
  pool_n <- min(nrow(z), max(n, ceiling(nrow(z) * 0.50)))
  pool <- z[seq_len(pool_n), , drop = FALSE]
  if (nrow(pool) <= n) return(pool)

  scale01 <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (diff(r) == 0) return(rep(0.5, length(x)))
    (x - r[1]) / diff(r)
  }
  xy <- cbind(scale01(pool$loss_percentile), scale01(pool$mitigation_percentile))
  chosen <- 1L
  while (length(chosen) < n) {
    remaining <- setdiff(seq_len(nrow(pool)), chosen)
    min_dist <- vapply(remaining, function(i) {
      min(sqrt(rowSums((xy[chosen, , drop = FALSE] - xy[i, ])^2)))
    }, numeric(1))
    best <- remaining[order(-min_dist, -pool$quadrant_extremeness[remaining])][1]
    chosen <- c(chosen, best)
  }
  pool[chosen, , drop = FALSE]
}
selected <- do.call(
  rbind,
  lapply(quadrant_order, function(q) pick_representative(city, q, 10))
)

pick_metric_top <- function(df, group_name, metric, n = 10) {
  z <- df[df$risk_quadrant == group_name, , drop = FALSE]
  if (!nrow(z)) return(z)
  z <- z[
    order(-z[[metric]], -z$baseline_loss_musd, -z$mitigation_loss_musd),
    , drop = FALSE
  ]
  z$strict_quadrant_rank <- seq_len(nrow(z))
  head(z, n)
}
if (selection_mode != "representative") {
  selection_metric <- if (selection_mode == "baseline_top10") {
    "baseline_loss_musd"
  } else {
    "remaining_loss_musd"
  }
  selected <- do.call(
    rbind,
    lapply(
      quadrant_order,
      function(q) pick_metric_top(city, q, selection_metric, 10)
    )
  )
}
selected$label_display <- sub(
  "([,_])([A-Z]{3})$", "\\n\\2", selected$city_label_plot
)

# A constrained 2-D swarm is used only for display. Exact zero-reduction
# observations occupy a labelled bottom display band; positive observations
# receive wider orthogonal jitter so proportional loss-reduction relationships
# appear as density clouds rather than artificial-looking rails. Analytical
# percentiles, quadrants and rankings remain unchanged in the exported tables.
set.seed(if (scenario == "SSP126") 2026126 else 2026370)
city$x_plot <- city$loss_percentile + rnorm(nrow(city), 0, 0.015)
city$y_plot <- city$mitigation_percentile + rnorm(nrow(city), 0, 0.055)
city$y_plot[!positive] <- runif(sum(!positive), 0.025, 0.235)
selected$x_plot <- selected$loss_percentile + rnorm(nrow(selected), 0, 0.020)
selected$y_plot <- selected$mitigation_percentile + rnorm(nrow(selected), 0, 0.065)
selected$y_plot[selected$mitigation_loss_musd == 0] <-
  runif(sum(selected$mitigation_loss_musd == 0), 0.045, 0.275)

constrain_quadrant <- function(df) {
  high_loss <- grepl("^High loss", as.character(df$risk_quadrant))
  high_mitigation <- grepl("high mitigation$", as.character(df$risk_quadrant))
  df$x_plot[high_loss] <- pmax(df$x_plot[high_loss], loss_threshold + 0.006)
  df$x_plot[!high_loss] <- pmin(df$x_plot[!high_loss], loss_threshold - 0.006)
  df$y_plot[high_mitigation] <- pmax(df$y_plot[high_mitigation], mitigation_threshold + 0.008)
  df$y_plot[!high_mitigation] <- pmin(df$y_plot[!high_mitigation], mitigation_threshold - 0.008)
  df$x_plot <- pmin(pmax(df$x_plot, 0.005), 0.995)
  df$y_plot <- pmin(pmax(df$y_plot, 0.005), 0.995)
  df
}
city <- constrain_quadrant(city)
selected <- constrain_quadrant(selected)

quad_colors <- c(
  "High loss + low mitigation" = "#C97A62",
  "High loss + high mitigation" = "#315B73",
  "Low loss + high mitigation" = "#789DA0",
  "Low loss + low mitigation" = "#AAA6A0"
)
quadrant_labels <- data.frame(
  x = c(0.985, 0.985, 0.015, 0.015),
  y = c(0.475, 0.525, 0.525, 0.475),
  hjust = c(1, 1, 0, 0), vjust = c(1, 0, 0, 1),
  label = c(
    "High loss\nLow mitigation", "High loss\nHigh mitigation",
    "Low loss\nHigh mitigation", "Low loss\nLow mitigation"
  ),
  risk_quadrant = factor(quadrant_order, levels = quadrant_order)
)
repel_layer <- function(group_name, xlim_use, ylim_use, seed_offset) {
  geom_text_repel(
    data = selected[selected$risk_quadrant == group_name, , drop = FALSE],
    aes(x = x_plot, y = y_plot, label = label_display),
    family = "sans", size = 3.0, color = "#303534",
    box.padding = 0.42, point.padding = 0.28,
    min.segment.length = 0, segment.color = "#A9AEAD",
    segment.size = 0.25, max.overlaps = Inf,
    seed = 20260717 + seed_offset, force = 16, force_pull = 0.05,
    max.iter = 200000, max.time = 25,
    xlim = xlim_use, ylim = ylim_use
  )
}
repel_layers <- list(
  repel_layer("High loss + high mitigation", c(loss_threshold, 1), c(mitigation_threshold, 1), 1),
  repel_layer("Low loss + high mitigation", c(0, loss_threshold), c(mitigation_threshold, 1), 2),
  repel_layer("Low loss + low mitigation", c(0, loss_threshold), c(0, mitigation_threshold), 3),
  repel_layer("High loss + low mitigation", c(loss_threshold, 1), c(0, mitigation_threshold), 4)
)

p <- ggplot() +
  geom_point(data = city, aes(x = x_plot, y = y_plot), color = "#B8C2C1", size = 0.95, alpha = 0.30) +
  geom_vline(xintercept = loss_threshold, color = "#8C9291", linewidth = 0.45, linetype = "22") +
  geom_hline(yintercept = mitigation_threshold, color = "#8C9291", linewidth = 0.45, linetype = "22") +
  geom_point(
    data = selected, aes(x = x_plot, y = y_plot, fill = risk_quadrant),
    shape = 21, size = 3.2, color = "white", stroke = 0.4,
    alpha = 0.98, show.legend = FALSE
  ) +
  repel_layers +
  geom_label(
    data = quadrant_labels,
    aes(x = x, y = y, label = label, color = risk_quadrant, hjust = hjust, vjust = vjust),
    family = "sans", fontface = "bold", size = 3.15,
    fill = alpha("white", 0.82), label.size = 0,
    label.padding = unit(0.12, "lines"), lineheight = 0.95,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.25),
    labels = label_percent(accuracy = 1), expand = expansion(mult = c(0.015, 0.015))
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.25),
    labels = label_percent(accuracy = 1), expand = expansion(mult = c(0.015, 0.015))
  ) +
  scale_fill_manual(values = quad_colors, drop = FALSE) +
  scale_color_manual(values = quad_colors, drop = FALSE) +
  labs(
    x = paste("Baseline-loss percentile", scenario_label, "2050", sep = " | "),
    y = "Avoided-loss percentile"
  ) +
  theme_classic(base_family = "sans", base_size = 12) +
  theme(
    plot.margin = margin(5, 7, 4, 5),
    axis.title = element_text(size = 11.5, face = "bold", color = "#252525"),
    axis.text = element_text(size = 10.5, color = "#303534"),
    axis.line = element_line(color = "#6E7473", linewidth = 0.45),
    panel.border = element_rect(color = "#8C9291", fill = NA, linewidth = 0.45),
    legend.position = "none"
  )

selection_tag <- if (selection_mode == "representative") {
  "representative10_each"
} else if (selection_mode == "baseline_top10") {
  "strict_baseline_loss_top10_each"
} else {
  "strict_remaining_loss_top10_each"
}
stem <- paste0(file.path(
  out_dir,
  paste0("city_quadrant_percentile_", selection_tag, "_", scenario, "_2050_scatter")
), plot_suffix)
ggsave(paste0(stem, ".svg"), p, width = plot_width, height = plot_height, units = "in", device = svglite)
ggsave(paste0(stem, ".pdf"), p, width = plot_width, height = plot_height, units = "in", device = "pdf")
ggsave(paste0(stem, ".png"), p, width = plot_width, height = plot_height, units = "in", dpi = 300)

summary_out <- data.frame(
  scenario = scenario, cities = nrow(city),
  baseline_loss_percentile_threshold = loss_threshold,
  avoided_loss_percentile_threshold = mitigation_threshold,
  baseline_loss_p80_musd = as.numeric(quantile(city$baseline_loss_musd, loss_threshold, names = FALSE)),
  positive_mitigation_median_musd = median(city$mitigation_loss_musd[city$mitigation_loss_musd > 0]),
  selection_mode = selection_mode,
  highlighted_cities = nrow(selected)
)
write.csv(summary_out, file.path(out_dir, paste0("quadrant_thresholds_", scenario, ".csv")), row.names = FALSE)
write.csv(
  strict_top[
    order(strict_top$risk_quadrant, strict_top$quadrant_rank),
    c(
      "risk_quadrant", "quadrant_rank", "city_label_plot",
      "baseline_loss_musd", "mitigation_loss_musd", "reduction_rate",
      "loss_percentile", "mitigation_percentile", "quadrant_extremeness"
    )
  ],
  file.path(out_dir, paste0("quadrant_strict_top10_corner_extremeness_", scenario, ".csv")),
  row.names = FALSE
)
write.csv(
  selected[
    order(selected$risk_quadrant, selected$strict_quadrant_rank),
    c(
      "risk_quadrant", "strict_quadrant_rank", "city_label_plot",
      "baseline_loss_musd", "mitigation_loss_musd", "reduction_rate",
      "loss_percentile", "mitigation_percentile", "quadrant_extremeness"
    )
  ],
  file.path(
    out_dir,
    paste0(
      if (selection_mode == "representative") {
        "quadrant_representative10_high_priority_"
      } else if (selection_mode == "baseline_top10") {
        "quadrant_highlighted_strict_baseline_loss_top10_"
      } else {
        "quadrant_highlighted_strict_remaining_loss_top10_"
      },
      scenario,
      ".csv"
    )
  ),
  row.names = FALSE
)
for (metric_name in c("baseline_loss_musd", "mitigation_loss_musd", "remaining_loss_musd")) {
  metric_top <- do.call(
    rbind,
    lapply(
      quadrant_order,
      function(q) pick_metric_top(city, q, metric_name, 10)
    )
  )
  write.csv(
    metric_top[
      order(metric_top$risk_quadrant, metric_top$strict_quadrant_rank),
      c(
        "risk_quadrant", "strict_quadrant_rank", "city_label_plot",
        "baseline_loss_musd", "mitigation_loss_musd",
        "remaining_loss_musd", "reduction_rate",
        "loss_percentile", "mitigation_percentile"
      )
    ],
    file.path(
      out_dir,
      paste0("quadrant_strict_top10_by_", metric_name, "_", scenario, ".csv")
    ),
    row.names = FALSE
  )
}
cat("Scenario:", scenario, "\nCities:", nrow(city), "\nSelected:", nrow(selected), "\n")

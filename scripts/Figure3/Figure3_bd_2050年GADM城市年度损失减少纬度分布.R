suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(svglite)
  library(rsvg)
})

base_dir <- "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图三退役与未来情景"
map_dir <- file.path(base_dir, "2050年GADM城市年度损失减少世界地图")
out_dir <- file.path(base_dir, "2050年GADM城市年度损失减少纬度分布")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

scenarios <- c("SSP126", "SSP370")
scenario_labels <- c(SSP126 = "SSP1-2.6", SSP370 = "SSP3-7.0")
lat_step <- 10
lat_breaks <- seq(-60, 80, by = lat_step)

latitude_labels <- function(y) {
  ifelse(
    y == 0, "0°",
    paste0(abs(y), "°", ifelse(y > 0, "N", "S"))
  )
}

make_profile <- function(scenario) {
  source_csv <- file.path(
    map_dir,
    paste0(
      "gadm_admin2_annual_loss_reduction_", scenario,
      "_2050_fig2_palette_summary.csv"
    )
  )
  dat <- read_csv(source_csv, show_col_types = FALSE) %>%
    transmute(
      latitude = suppressWarnings(as.numeric(fallback_lat)),
      annual_loss_reduction_musd =
        suppressWarnings(as.numeric(avoided_loss_musd))
    ) %>%
    filter(
      is.finite(latitude),
      is.finite(annual_loss_reduction_musd),
      annual_loss_reduction_musd > 0,
      latitude >= min(lat_breaks),
      latitude < max(lat_breaks)
    ) %>%
    mutate(
      lat_index = findInterval(
        latitude, lat_breaks, rightmost.closed = FALSE
      ),
      latitude_center = lat_breaks[lat_index] + lat_step / 2
    )

  profile <- dat %>%
    group_by(latitude_center) %>%
    summarise(
      latitude = first(latitude_center),
      n = n(),
      mean = mean(annual_loss_reduction_musd),
      sd = sd(annual_loss_reduction_musd),
      median = median(annual_loss_reduction_musd),
      q25 = quantile(annual_loss_reduction_musd, 0.25),
      q75 = quantile(annual_loss_reduction_musd, 0.75),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>%
    mutate(
      sd = if_else(is.finite(sd), sd, 0),
      ribbon_min = pmax(mean - sd, 0),
      ribbon_max = mean + sd,
      scenario = .env$scenario,
      scenario_label = scenario_labels[[.env$scenario]]
    ) %>%
    arrange(latitude)

  write_csv(
    profile,
    file.path(
      out_dir,
      paste0(
        "annual_loss_reduction_latitudinal_profile_",
        scenario, "_2050.csv"
      )
    )
  )

  p <- ggplot(profile, aes(y = latitude)) +
    geom_ribbon(
      aes(xmin = ribbon_min, xmax = ribbon_max),
      fill = "#C8D7D8", alpha = 0.72, orientation = "y"
    ) +
    geom_path(aes(x = mean), color = "#315B73", linewidth = 0.9) +
    scale_y_continuous(
      limits = c(-60, 80),
      breaks = c(-30, 0, 30, 60),
      labels = latitude_labels,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(
      labels = label_number(accuracy = 0.1),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    labs(
      x = "Mean annual loss reduction\n(M USD city^-1)",
      y = "Latitude"
    ) +
    theme_classic(base_size = 10, base_family = "Arial") +
    theme(
      panel.border = element_rect(
        fill = NA, color = "#555555", linewidth = 0.4
      ),
      axis.line = element_blank(),
      axis.text = element_text(size = 8.8, color = "#3F3F3F"),
      axis.title = element_text(size = 9.5, color = "#202020"),
      axis.ticks = element_line(color = "#666666", linewidth = 0.3),
      plot.margin = margin(4, 14, 4, 4)
    )

  stem <- file.path(
    out_dir,
    paste0(
      "annual_loss_reduction_latitudinal_profile_",
      scenario, "_2050"
    )
  )
  ggsave(
    paste0(stem, ".svg"), p,
    width = 2.75, height = 4.60, device = svglite::svglite
  )
  rsvg_pdf(paste0(stem, ".svg"), paste0(stem, ".pdf"))
  rsvg_png(paste0(stem, ".svg"), paste0(stem, ".png"), width = 1000)
  profile
}

profiles <- bind_rows(lapply(scenarios, make_profile))
write_csv(
  profiles,
  file.path(
    out_dir,
    "annual_loss_reduction_latitudinal_profiles_SSP126_SSP370_2050.csv"
  )
)

cat("Saved profiles in:\n", out_dir, "\n", sep = "")

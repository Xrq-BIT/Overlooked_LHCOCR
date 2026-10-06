# =========================
# 0) Packages
# =========================
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(akima)
  library(stringr)
  library(cowplot)
  library(svglite)
  library(grid)
})

# 关键：关闭 s2，避免无效 polygon 在 s2 下报错
sf::sf_use_s2(FALSE)

# =========================
# 1) File paths
# =========================
tif_path <- "/Users/xuruiqing/Downloads/plant_airtemp_polybands_2019_id_100000103891.tif"
shp_path <- "/Users/xuruiqing/Desktop/文章试验整体流程/data/all_7400_with_area/all_7400_with_area.shp"

# 输出文件
OUT_MAP_SVG <- "44case_temp_map100000103891.svg"
OUT_LEG_SVG <- "44case_temp_legend100000103891.svg"
OUT_COMBINED_SVG <- "44case_temp_combined100000103891.svg"

# 输出尺寸
MAP_W <- 5.2
MAP_H <- 5.2
LEG_W <- 1.4
LEG_H <- MAP_H

T_MIN <- 5
T_MAX <- 35

CASE_LABEL <- "Bituminous (Sullivan, USA)"

# =========================
# 2) 从 tif 文件名里提取电厂 id
# =========================
tif_name <- basename(tif_path)
target_id <- str_match(tif_name, "id_(\\d+)\\.tif$")[, 2]

if (is.na(target_id)) {
  stop("无法从 tif 文件名中提取 id，请检查文件名格式。")
}

# Manual display labels used in the manuscript; raw geography remains in Excel.
case_labels <- c(
  "100000100221" = "Other coal (Huaibei, CHN)",
  "100000103708" = "Lignite (Kangal, TUR)",
  "100000101668" = "Bituminous (Changji Hui, CHN)",
  "100000103891" = "Bituminous (Sullivan, USA)"
)
if (target_id %in% names(case_labels)) CASE_LABEL <- unname(case_labels[target_id])

cat("Target id =", target_id, "\n")

# =========================
# 3) Read tif
# =========================
r <- rast(tif_path)
print(r)
print(names(r))

# 假设 band1 = T
T_r <- r[[1]]

# =========================
# 4) 原始温度点
# =========================
df_raw <- as.data.frame(T_r, xy = TRUE, na.rm = TRUE)
names(df_raw) <- c("x", "y", "T")

# =========================
# 5) 做更细的展示插值
# =========================
xo <- seq(min(df_raw$x), max(df_raw$x), length.out = 250)
yo <- seq(min(df_raw$y), max(df_raw$y), length.out = 250)

interp_res <- akima::interp(
  x = df_raw$x,
  y = df_raw$y,
  z = df_raw$T,
  xo = xo,
  yo = yo,
  linear = FALSE,
  extrap = FALSE,
  duplicate = "mean"
)

df_T <- expand.grid(
  x = interp_res$x,
  y = interp_res$y
)

df_T$T <- as.vector(interp_res$z)

df_T <- df_T %>%
  filter(!is.na(T))

# =========================
# 6) 读取真实 shp，并按 id 匹配 polygon
# =========================
all_poly <- st_read(shp_path, quiet = TRUE)
print(names(all_poly))

id_field <- "id"

if (!(id_field %in% names(all_poly))) {
  stop(paste0("shp 中不存在字段：", id_field))
}

all_poly[[id_field]] <- as.character(all_poly[[id_field]])
target_id <- as.character(target_id)

poly_sf <- all_poly %>%
  filter(.data[[id_field]] == target_id)

if (nrow(poly_sf) == 0) {
  stop(paste0("在 shp 中没有找到 id = ", target_id, " 的 polygon"))
}

# 投影到 tif 坐标系
poly_sf <- st_transform(poly_sf, crs = crs(r))

# 修复无效几何
poly_sf <- st_make_valid(poly_sf)
poly_sf <- st_buffer(poly_sf, 0)

# 只保留 POLYGON / MULTIPOLYGON
poly_sf <- st_collection_extract(poly_sf, "POLYGON", warn = FALSE)

if (nrow(poly_sf) == 0) {
  stop("修复几何后，没有可用的 POLYGON 几何。")
}

# 如果一个 id 对应多个部件，合并成一个对象
if (nrow(poly_sf) > 1) {
  poly_geom <- st_union(st_geometry(poly_sf))
  poly_sf <- st_sf(geometry = poly_geom)
}

# =========================
# 6.5) 裁剪显示范围
# =========================
HALF_SIZE_SHOW <- 4000   # 4000 = 8km
# HALF_SIZE_SHOW <- 3500 # 3500 = 7km

poly_center <- st_point_on_surface(poly_sf)

center_3857 <- st_transform(poly_center, 3857)

square_show_3857 <- st_buffer(center_3857, HALF_SIZE_SHOW) |>
  st_bbox() |>
  st_as_sfc()

square_show <- st_transform(square_show_3857, crs = crs(r))

# =========================
# 7) 生成 1/2/3/4 km 外扩边界线
# =========================
poly_3857 <- st_transform(poly_sf, 3857)

buf1 <- st_buffer(poly_3857, 1000)
buf2 <- st_buffer(poly_3857, 2000)
buf3 <- st_buffer(poly_3857, 3000)
buf4 <- st_buffer(poly_3857, 4000)

buf1 <- st_transform(buf1, crs = crs(r))
buf2 <- st_transform(buf2, crs = crs(r))
buf3 <- st_transform(buf3, crs = crs(r))
buf4 <- st_transform(buf4, crs = crs(r))

# =========================
# 8) Plot：主图，不带图例，不带 caption
# =========================
p <- ggplot() +
  geom_raster(
    data = df_T,
    aes(x = x, y = y, fill = T)
  ) +
  
  # 4km -> 1km 从外到内画，避免遮挡
  geom_sf(
    data = buf4,
    fill = NA,
    color = "black",
    linewidth = 0.35,
    alpha = 0.55,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = buf3,
    fill = NA,
    color = "black",
    linewidth = 0.40,
    alpha = 0.60,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = buf2,
    fill = NA,
    color = "black",
    linewidth = 0.45,
    alpha = 0.70,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = buf1,
    fill = NA,
    color = "black",
    linewidth = 0.50,
    alpha = 0.80,
    inherit.aes = FALSE
  ) +
  geom_sf(
    data = poly_sf,
    fill = NA,
    color = "black",
    linewidth = 1.10,
    inherit.aes = FALSE
  ) +
  coord_sf(
    xlim = st_bbox(square_show)[c("xmin", "xmax")],
    ylim = st_bbox(square_show)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  scale_fill_gradientn(
    colours = c(
      "#5779CE", "#69ABD3", "#82C1C4", "#C2DFC0",
      "#F0F0C6", "#F6D58F", "#F3A25F", "#EA704C",
      "#CC3B34", "#A72B61"
    ),
    name = "Temp (°C)",
    guide = "none"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.6),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(8, 8, 8, 8)
  )

print(p)

# =========================
# 9) 单独做图例
# =========================
p_leg_src <- ggplot() +
  geom_raster(
    data = df_T,
    aes(x = x, y = y, fill = T)
  ) +
  scale_fill_gradientn(
    colours = c(
      "#5779CE", "#69ABD3", "#82C1C4", "#C2DFC0",
      "#F0F0C6", "#F6D58F", "#F3A25F", "#EA704C",
      "#CC3B34", "#A72B61"
    ),
    name = "Temp (°C)",
    guide = guide_colorbar(
      barheight = unit(110, "mm"),
      barwidth  = unit(7, "mm"),
      ticks.colour = "grey30",
      frame.colour = "grey30"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "left",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 11)
  )

leg <- cowplot::get_legend(p_leg_src)

# =========================
# 10) 导出主图和单独图例
# =========================
ggsave(
  OUT_MAP_SVG,
  p,
  width = MAP_W,
  height = MAP_H,
  device = svglite::svglite
)

ggsave(
  OUT_LEG_SVG,
  leg,
  width = LEG_W,
  height = LEG_H,
  device = svglite::svglite
)

# =========================
# 11) 合成主图 + 图例 + 底部文字
#     温度图和色标同底同高，文字单独放在下方
# =========================
LEG_REL_W <- 0.22
GAP <- 0

# 调小这个值：底部文字区更矮，文字更靠近正方形主图
LABEL_H <- 0.055
MAP_AREA_H <- 1 - LABEL_H

p_combined <- cowplot::ggdraw() +
  
  # 主图：只占上方区域
  cowplot::draw_plot(
    p,
    x = 0,
    y = LABEL_H,
    width = 1 - LEG_REL_W - GAP,
    height = MAP_AREA_H
  ) +
  
  # 图例：和主图同底同高
  cowplot::draw_grob(
    leg,
    x = 1 - LEG_REL_W,
    y = LABEL_H,
    width = LEG_REL_W,
    height = MAP_AREA_H
  ) +
  
  # 底部文字：更靠近主图
  cowplot::draw_label(
    CASE_LABEL,
    x = (1 - LEG_REL_W - GAP) / 2,
    y = LABEL_H * 1,
    size = 13,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5
  )

# =========================
# 12) 导出组合图
# =========================
ggsave(
  OUT_COMBINED_SVG,
  p_combined,
  width = MAP_W + LEG_W,
  height = MAP_H + 0.25,
  device = svglite::svglite
)
rsvg::rsvg_pdf(OUT_COMBINED_SVG, sub("\\.svg$", ".pdf", OUT_COMBINED_SVG))

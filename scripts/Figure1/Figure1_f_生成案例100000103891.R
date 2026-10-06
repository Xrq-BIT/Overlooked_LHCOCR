# Reuse the accepted case-map script with case-specific inputs.
template_path <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/论文图件与SourceData/组图一/",
  "Figure1_cdef_具体厂温度等温线环状图导出svg.R"
)
output_dir <- paste0(
  "/Users/xuruiqing/Desktop/文章试验整体流程/绘图/图一温差统计/",
  "case_temperature_fields"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

script <- readLines(template_path, warn = FALSE, encoding = "UTF-8")
script <- sub(
  '^tif_path <- ".*"$',
  'tif_path <- "/Users/xuruiqing/Downloads/plant_airtemp_polybands_2019_id_100000103891.tif"',
  script
)
script <- sub(
  '^OUT_MAP_SVG <- ".*"$',
  'OUT_MAP_SVG <- "44case_temp_map100000103891.svg"',
  script
)
script <- sub(
  '^OUT_LEG_SVG <- ".*"$',
  'OUT_LEG_SVG <- "44case_temp_legend100000103891.svg"',
  script
)
script <- sub(
  '^OUT_COMBINED_SVG <- ".*"$',
  'OUT_COMBINED_SVG <- "44case_temp_combined100000103891.svg"',
  script
)
script <- sub(
  '^CASE_LABEL <- ".*"$',
  'CASE_LABEL <- "Bituminous (Sullivan, USA)"',
  script
)

old_wd <- setwd(output_dir)
on.exit(setwd(old_wd), add = TRUE)
eval(parse(text = script), envir = new.env(parent = globalenv()))

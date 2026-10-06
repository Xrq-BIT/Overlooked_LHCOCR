args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
base <- if (length(script)) dirname(normalizePath(script[1])) else getwd()
files <- c(
  list.files(file.path(base, "组图一"), "^Fig1[cdef]_.*[.]svg$", full.names = TRUE),
  list.files(file.path(base, "组图二"), "^Fig2[de]_.*[.]svg$", full.names = TRUE)
)
stopifnot(length(files) == 6L)
for (path in files) {
  output <- sub("[.]svg$", ".pdf", path)
  rsvg::rsvg_pdf(path, output)
  message(output)
}

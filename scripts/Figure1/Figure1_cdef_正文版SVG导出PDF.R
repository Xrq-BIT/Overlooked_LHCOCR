# Export the supplied final-layout SVGs. No temperature data are recalculated.
suppressPackageStartupMessages(library(rsvg))
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
folder <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else getwd()
files <- list.files(folder, pattern = "^Fig1[c-f]_44case_temp_combined[0-9]+\\.svg$", full.names = TRUE)
stopifnot(length(files) == 4L)
for (f in files) rsvg_pdf(f, sub("\\.svg$", ".pdf", f))

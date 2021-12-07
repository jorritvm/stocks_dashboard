lapply(list.files(path = "R", recursive = FALSE, pattern = "\\.R$", full.names = TRUE), source)
source("load_libraries.R")

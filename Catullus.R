setwd("~/OneDrive/GithubProjects/ToPan")

# vol.number <- "1"
# ocr_edition <- "coo.31924054872803_ocr"

# path_name <- paste('./data', vol.number, "/", ocr_edition, '/', sep = "")
path_name <- './Catullus//'

# files <- list.files(path = path_name, pattern = "[.]txt$")
files <- list.files(path = path_name, pattern = "[.]txt$", recursive = TRUE)

page_numbers <- gsub('[[:alpha:]]', '', files)
files <- paste(path_name, files, sep = "")

library(doParallel)
registerDoParallel(cores = 4)
library(foreach)

data <- foreach(i = files, .combine = rbind) %dopar% {
  list(i, paste(scan(file = i, what = character(), sep = '\n'), collapse = " "))
}
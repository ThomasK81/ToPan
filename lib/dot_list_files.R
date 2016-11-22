#
# Stolen from https://github.com/betabandido/exptools/blob/master/R/util.R with gratitude!
#

library(stringr)

assert <- function(expr, msg) {
  if (!expr) stop(msg, call. = F)
}

# Have it not require a second argument, instead listing all files by default. –jrs
.list.files <- function(path, pattern = '.*') {
  assert(is.character(path) && path != '',
         'path must be a non-empty string')
  assert(is.character(pattern) && pattern != '',
         'pattern must be a non-empty string')

  fname.pattern <- utils::tail(strsplit(pattern, '/')[[1]], n = 1)
  file.list <- base::list.files(path,
                                fname.pattern,
                                recursive = T,
                                full.names = T)
  if (length(file.list) > 0) {
    # Returns TRUE if the path matches the pattern. For that to happen,
    # the complete match and all the capture groups must be valid.
    matches.pattern <- function(path) {
      m <- str_match(path, pattern)
      all(is.na(m) == FALSE)
    }
    file.list <- file.list[sapply(file.list, matches.pattern)]
  }

  return(file.list)
}

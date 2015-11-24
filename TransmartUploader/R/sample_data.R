



#' fetch the Test4 sample data
#'
#' @return a named list of data frames
#'
#' @author karl
#' @export
fetch_test4_sample_data <- function() {
  dir <- find_extdata_file('sample_data/test4')
  files <- dir(dir, full.names = TRUE)

  parse_file <- function(x) read.table(x, header = TRUE, sep = "\t",
    check.names = FALSE, stringsAsFactors = FALSE)

  dfs <- lapply(files, parse_file)

  names(dfs) <- basename(files)

  dfs
}


#' fetch the fam test file
#'
#' @return a file path
#'
#' @author karl
#' @export
fetch_ref_fam <- function() { find_extdata_file('sample_data/fam/ref.fam') }
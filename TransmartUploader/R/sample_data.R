



#' fetch the Test4 sample data
#'
#' @return a named list of data frames
#'
#' @author karl
#' @export
fetch_test4_sample_data <- function() {
  dir <- find_extdata_file('sample_data/test4')
  files <- dir(dir, full.names = TRUE)
  dfs <- lapply(files, read.table, header = TRUE, sep = "\t",
    check.names = FALSE,
    stringsAsFactors = FALSE)

  names(dfs) <- basename(files)

  dfs
}
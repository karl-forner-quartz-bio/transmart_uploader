
#' fetch the tMDataLoader sample "Low Dimentional Serial Data Test"
#'
#' this sample from \url{https://github.com/Clarivate-LSPS/tMDataLoader-samples}
#' have been integrated into this package for testing purposes
#'
#'
#' @return the path of the directory containing the sample test study
#'
#' @author karl
#' @keywords internal
fetch_lowdimserialdata_sample <- function() {
  find_extdata_file('sample_data/lowdimserialdata')
}


#' fetch the tMDataLoader sample "Test\ aCGH\ Copy\ Number\ Variations" clinical data
#'
#' this sample from \url{https://github.com/Clarivate-LSPS/tMDataLoader-samples}
#' have been integrated into this package for testing purposes
#'
#' @return the path of the directory containing the sample test study
#' @author karl
#' @keywords internal
fetch_acgh_cnv_clinical_sample <- function() {
  find_extdata_file('sample_data/acgh_cnv_clinical')
}


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





#' fetch the tMDataLoader-samples
#'
#' samples from \url{https://github.com/Clarivate-LSPS/tMDataLoader-samples}
#' have been integrated into this package for testing purposes
#'
#' It contains in particular a mispelled 'Low Dimentional Serial Data Test'
#'
#' @return the path of the directory containing the sample test studies
#'
#' @author karl
#' @export
fetch_tMDataLoader_samples <- function() {
  find_extdata_file('sample_data/tMDataLoader-samples')
}

#' upload atMDataLoader-sample
#'
#' @keywords internal
upload_tMDataLoader_sample <- function(sample_dir, etl_path, study_id, ...) {
  test_dir <- normalizePath(fetch_tMDataLoader_samples())

  setup_temp_dir()

  etl_test <- file.path('ETL', etl_path)
  dir.create(etl_test, recursive = TRUE)
  file.copy(file.path(test_dir, sample_dir), etl_test, recursive = TRUE)

  create_etl_config('Config.groovy', data_dir = 'ETL', ...)

  execute_etl_cmd('Config.groovy')
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




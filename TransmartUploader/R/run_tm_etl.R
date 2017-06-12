

#' run the tMDataLoader tool
#'
#' Will reformat the data and generate automatically the Mapping file using
#' the supplied mapper map_df
#'
#' @inheritParams params
#' @inheritParams run_tm_etl_on_processed_data
#' @param ...					additional arguments to \code{run_etl_command}
#'
#' @return the upload output log and summary statistics as a list
#'
#' @author karl
#' @export
run_tm_etl <- function(
  data_dfs,
  map_df,
  etl_path,
  ...
) {

  if (inherits(data_dfs, 'data.frame')) data_dfs <- list(data_dfs)

  # fetch study_id
  study_id <- unique(data_dfs[[1]]$STUDY_ID)
  if (is.null(study_id) || !nzchar(study_id)) {
    stop('STUDY_ID is MANDATORY')
  }

  # prepare data
  data_dfs <- lapply(data_dfs, format_input_data)
  filenames <- sprintf('%s_%i.txt', study_id, seq_along(data_dfs))
  map_file_df <- generate_mapping(data_dfs, map_df, filenames)

  run_tm_etl_on_processed_data(data_dfs, map_file_df, etl_path = etl_path, ...)
}


#' run the tMDataLoader tool on already formatted and mapped data
#'
#' @param dir		directory in which to create the input files
#' 	and to execute the ETL
#' @inheritParams params
#' @param ...					additional arguments to \code{run_etl_command}
#' @return the upload output log and summary statistics as a list
#' @author karl
#' @export
run_tm_etl_on_processed_data <- function(
  data_dfs,
  map_file_df,
  etl_path,
  dir = NULL,
  data_dir = 'ETL',
  host = 'localhost',
  port = 5432,
  ...)
{
  if (inherits(data_dfs, 'data.frame')) data_dfs <- list(data_dfs)

  ### setup the working dir
  if (is.null(dir)) {
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  } else {
    if (!file.exists(dir)) stop('Error, dir does not exist: ', dir)
  }

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  ### input files
  study_id <- unique(data_dfs[[1]]$STUDY_ID)
  if (is.null(study_id) || !nzchar(study_id)) {
    stop('STUDY_ID is MANDATORY')
  }
  write_etl_files(data_dfs, map_file_df, data_dir, etl_path, study_id)

  ### config file
  config_file <- 'Config.groovy'
  create_etl_config(config_file, host = host, port = port,
    data_dir = data_dir)

  out <- execute_etl_cmd(config_file, ...)

  stats <- fetch_etl_summary_statistics(file.path(data_dir, etl_path))

  invisible(list(output = out, stats = stats))
}









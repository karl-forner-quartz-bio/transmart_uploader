

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
  data_dfs <- lapply(data_dfs, format_data_for_tmdataloader, map_df)
  filenames <- sprintf('%s_%i.txt', study_id, seq_along(data_dfs))
  map_file_df <- build_tmdataloader_mapping_file(data_dfs, map_df, filenames)

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
#' @keywords internal
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
    if (!file.exists(dir)) stop('Error, dir does not exit: ', dir)
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





# this function reorders the df so that the first 2 columns
# are the STUDY_ID and SUBJ_ID, and remove unmapped columns
format_data_for_tmdataloader <- function(df, mapping_df) {
  STUDY_ID <- 'STUDY_ID'
  SUBJ_ID <- 'SUBJ_ID'

  if (!STUDY_ID %in% names(df)) stop("STUDY_ID column is missing")
  if (!SUBJ_ID %in% names(df)) stop("SUBJ_ID column is missing")

  # remove columns not in mapping
  cols <- intersect(names(df), mapping_df$data_label)
  df <- df[, cols]

  # reorder the columns
  mandatory <- c(STUDY_ID, SUBJ_ID)
  newcols <- c(mandatory, setdiff(names(df), mandatory))

  df <- df[, newcols]

  df
}



# generate the mapping file for tMDataLoader as a data frame
# N.B: it is ordered in the same order than data_dfs
build_tmdataloader_mapping_file <- function(data_dfs, mapper, filenames) {
  vars <- mapper$data_label
  # 1. remove columns not in the mapping_df
  for (i in 1:length(data_dfs)) {
    cols <- intersect(names(data_dfs[[i]]), vars)
    data_dfs[[i]] <- data_dfs[[i]][, cols, drop = FALSE]
  }

  # make a subset of the mapping file
  cols <- unique(unlist(lapply(data_dfs, names), use.names = FALSE))
  map <- mapper[mapper$data_label %in% cols, ]


  # now create the tMDataLoader mapping file with columns filename and col_nbr
  .make_mapping <- function(i) {
    df <- data_dfs[[i]]
    mm <- data.frame(data_label = names(df), col_nbr = 1:ncol(df),
      stringsAsFactors = FALSE)
    res <- merge(mm, map, by = 'data_label')
    res$filename <- filenames[i]
    res <- res[, c('filename',	'category_cd', 	'col_nbr', 'data_label')]

    # order by col_nbr
    res[order(res$col_nbr), ]
  }

  res <- lapply(seq_along(data_dfs), .make_mapping)
  res <- do.call(rbind, res)

  res
}


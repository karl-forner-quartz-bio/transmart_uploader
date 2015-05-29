

#' run the tMDataLoader tool
#'
#' @param data_dfs		the data to upload
#' @param map_df			the corresponding mapping
#' @param etl_path		the path in etl tree, as a standard path string
#' @param host				the transmart DB host
#' @param port				the transmart DB port
#' @param java				the java executable to use
#' @param jar					the tMDataLoader jar file
#' @param config			the tMDataLoader config file template
#' @param dir					the directory in which to setup the data files
#' 										to upload. If not given will occur in a temp dir
#' @param ...					additional arguments to \code{execute_etl_cmd}
#'
#' @return the upload summary statistics as a data frame, or NULL
#'
#' @author karl
#' @export
run_tm_etl <- function(
  data_dfs,
  map_df,
  etl_path,
  host = 'localhost',
  port = 5432,
  java = 'java',
  jar = find_extdata_file('tm_etl.jar'),
  config = find_extdata_file('Config.groovy.brew'),
  dir = NULL,
  ...
) {
  check_jar(java, jar)

  # get study_id
  study_id <- unique(data_dfs[[1]]$STUDY_ID)
  if (is.null(study_id) || !nzchar(study_id)) {
    stop('STUDY_ID is MANDATORY')
  }

  # prepare data
  data_dfs <- lapply(data_dfs, format_data_for_tmdataloader, map_df)
  filenames <- sprintf('%s_%i.txt', study_id, seq_along(data_dfs))
  map_file_df <- build_tmdataloader_mapping_file(data_dfs, map_df, filenames)

  if (is.null(dir)) {
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  } else {
    if (!file.exists(dir)) stop('Error, dir does not exit: ', dir)
  }

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  ## write config
  config_file <- 'Config.groovy'
  data_dir <- 'ETL'
  brew(config, config_file) # N.B: data_dir must be set before

  setup_etl_files(data_dfs, map_file_df, data_dir, etl_path, study_id)

  out <- execute_etl_cmd(java, jar, config_file, ...)

  stats <- fetch_etl_summary_statistics(file.path(data_dir, etl_path))

  invisible(stats)
}

execute_etl_cmd <- function(java, jar, config_file, extra = '') {
  args <- sprintf('-jar %s -c %s -i -s %s', jar, config_file, extra)
  out <- system2(java, args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, 'status')
  if (!is.null(status)) {
    msg <- sprintf('There was a problem running "%s %s", status=%i', java, args,
      status)
    msg <- paste0(msg, '\noutput: \n', paste0(out, collapse = '\n'))
    stop(msg)
  }

  out
}


#' delete a study by id using the tMDataLoader tool
#'
#' @param id								the id of the study to delete
#' @inheritParams run_tm_etl
#'
#' @return the command output
#'
#' @author karl
#' @export
delete_study_by_id <- function(
  id,
  host = 'localhost',
  port = 5432,
  java = 'java',
  jar = find_extdata_file('tm_etl.jar'),
  config = find_extdata_file('Config.groovy.brew')
) {

  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  ## write config
  config_file <- 'Config.groovy'
  data_dir <- 'dummy'
  brew(config, config_file) # N.B: data_dir must be set before

  extra <- paste('--delete-study-by-id', id)
  out <- execute_etl_cmd(java, jar, config_file, extra = extra)

  invisible(out)
}



setup_etl_files <- function(data_dfs, map_df, data_dir, etl_path, prefix) {

  # make transmart path
  path <- file.path(data_dir, etl_path)
  dir.create(path, recursive = TRUE)

  # must be in the same order then dfs
  filenames <- file.path(path, unique(map_df$filename))
  for (i in seq_along(filenames)) {
    write.table(data_dfs[[i]], filenames[[i]],
      sep = '\t', quote = FALSE, row.names = FALSE)
  }

  map_path <- file.path(path, paste0(prefix, '_Mapping_File.txt'))
  write.table(map_df, map_path, sep = '\t', quote = FALSE, row.names = FALSE)

}


fetch_etl_summary_statistics <- function(etl_path) {
  dirs <- strsplit(etl_path, '/')[[1]]
  # add _DONE_ to data and study dirs
  n <- length(dirs)
  dirs[(n-1):n] <- paste0('_DONE_', dirs[(n-1):n])
  path <- paste0(dirs, collapse = '/')

  if (!file.exists(path)) return(NULL)
  stat_file <- file.path(path, 'SummaryStatistic.txt')

  stats <- read.table(stat_file, header = TRUE, sep = "\t", check.names = FALSE,
    stringsAsFactors = FALSE)

  stats
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



# generate the mapping for tMDataLoader as a data frame
# N.B: it is ordered in the same order than data_dfs
build_tmdataloader_mapping_file <- function(data_dfs, mapping_df, filenames) {
  vars <- mapping_df$data_label
  # 1. remove columns not in the mapping_df
  for (i in 1:length(data_dfs)) {
    cols <- intersect(names(data_dfs[[i]]), vars)
    data_dfs[[i]] <- data_dfs[[i]][, cols, drop = FALSE]
  }

  # make a subset of the mapping file
  cols <- unique(unlist(lapply(data_dfs, names), use.names = FALSE))
  map <- mapping_df[mapping_df$data_label %in% cols, ]


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


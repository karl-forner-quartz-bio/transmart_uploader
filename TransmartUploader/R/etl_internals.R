### functions that deal directly with tMdataloader

#' create the ETL config
#'
#' The ETL uses a confuration file (Config.groovy) for example for
#' the DB settings.
#'
#' @param path			  the path of config file to create
#' @param config			the tMDataLoader config file template
#' @return the config file path
#' @inheritParams params
#' @keywords internal
#' @author karl
create_etl_config <- function(
  config_file_path = tempfile('Config.groovy'),
  host = 'localhost',
  port = 5432,
  data_dir = 'ETL',
  config = find_extdata_file('Config.groovy.brew'))
{
  force(data_dir)
  force(host)
  force(port)
  # N.B: brew makes use of data_dir, host and port
  brew(config, config_file_path) # N.B: data_dir must be set before

  invisible(config_file_path)
}



#' utility to run a command using the tMDataLoader tool
#'
#' generate the config file, then execute the ETL command
#' in the current directory
#'
#' @inheritParams create_etl_config
#' @param postprocess	function to call after
#' @param ...					additional arguments to \code{create_etl_config}
#'
#' @return a list of the ETL output,
#' 	and the postprocess output if any
#'
#' @author karl
#' @keywords internal
execute_etl_cmd <- function(
  config_file = create_etl_config(...),
  java = 'java',
  jar = find_extdata_file('tm_etl.jar'),
  extra = '',
  ...)
 {
  check_jar(java, jar)

  # args <- sprintf('-jar %s -c %s -i -s %s', jar, config_file, extra)
  args <- sprintf('-jar %s -c %s -i -s %s --visit-name-first', jar, config_file, extra)

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


#' write the data as files for the ETL
#'
#'
#' @inheritParams params
#' @param prefix	the prefix of the Mapping file
#' @author karl
#' @keywords internal
write_etl_files <- function(data_dfs, map_df, data_dir, etl_path, prefix) {

  # make transmart path
  path <- file.path(data_dir, etl_path)
  dir.create(path, recursive = TRUE)

  # must be in the same order then dfs
  filenames <- file.path(path, unique(map_df$filename))
  for (i in seq_along(filenames)) {
    write_etl_data_file(data_dfs[[i]], filenames[[i]])
  }

  map_path <- file.path(path, paste0(prefix, '_Mapping_File.txt'))
  write_etl_mapping_file(map_df, map_path)

  invisible()
}

write_etl_mapping_file <- function(map_df, path, merge = attr(map_df, 'merge')) {
  con <- file(path, 'wt')
  on.exit(close(con), add = TRUE)

  if (!is.null(merge) && nzchar(merge[1])) {
    merge <- merge[1]
    cat('#MERGE_MODE: ', merge, '\n', sep = '', file = con)
  }
  write.table(map_df, con, sep = '\t', quote = FALSE, row.names = FALSE)
}


#' write data as an ETL text file
#'
#' all columns are converted to character, and NA are replaced by empty strings
#'
#' @inheritParams params
#' @param df 		the data to write
#' @param path	path of the file to write
#' @author karl
#' @keywords internal
write_etl_data_file <- function(df, path) {

  # all columns to character
  for (i in seq_along(df)) df[[i]] <- as.character(df[[i]])

  #NA to ''
  df[is.na(df)] <- ''

  write.table(df, path, sep = '\t', quote = FALSE, row.names = FALSE)

  invisible()
}

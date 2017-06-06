### functions that deal directly with tMdataloader

#' create the ETL config
#'
#' The ETL uses a confuration file (Config.groovy) for example for
#' the DB settings.
#'
#' @param path			  the path of config file to create
#' @param config			the tMDataLoader config file template
#' @inheritParams params
#' @keywords internal
#' @author karl
create_etl_config <- function(
  path,
  host = 'localhost',
  port = 5432,
  data_dir = 'dummy',
  config = find_extdata_file('Config.groovy.brew'))
{
  force(data_dir)
  force(host)
  force(port)
  # N.B: brew makes use of data_dir, host and port
  brew(config, path) # N.B: data_dir must be set before
}



#' utility to run a command using the tMDataLoader tool
#'
#' @inheritParams create_etl_config
#' @param postprocess	function to call after
#' @param ...					additional arguments to \code{execute_etl_cmd}
#'
#' @return a list if the execute_etl_cmd output,
#' 	and the postprocess output if any
#'
#' @author karl
#' @keywords internal
run_etl_command <- function(
  host = 'localhost',
  port = 5432,
  java = 'java',
  jar = find_extdata_file('tm_etl.jar'),
  data_dir = 'dummy',
  dir = NULL,
  preprocess = NULL,
  postprocess = NULL,
  ...
) {
  check_jar(java, jar)

  if (is.null(dir)) {
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  } else {
    if (!file.exists(dir)) stop('Error, dir does not exit: ', dir)
  }

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  config_file <- 'Config.groovy'
  create_etl_config(config_file, host = host, port = port,
    data_dir = data_dir)

  if (!is.null(preprocess)) preprocess()

  out <- execute_etl_cmd(java, jar, config_file, ...)

  res <- list(execute_etl_cmd = out)

  if (!is.null(postprocess)) res$postprocess <- postprocess()

  res
}


execute_etl_cmd <- function(java, jar, config_file, extra = '') {
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



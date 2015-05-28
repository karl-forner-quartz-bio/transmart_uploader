
find_extdata_file <- function(subpath) {
  path <- system.file(file.path('extdata', subpath), package = 'TransmartUploader')

  if (!nzchar(path) || !file.exists(path)) {
    stop(sprintf('unable to find extdata file "%s"', subpath))
  }

  path
}



#' run the tMDataLoader tool
#'
#' @param data_dfs			the data to upload
#' @param mapping_df		the corresponding mapping
#' @param host					the transmart DB host
#' @param port					the transmart DB port
#' @param java					the java executable to use
#' @param jar						the tMDataLoader jar file
#' @param config				the tMDataLoader config file template
#' @param dir						the directory in which to setup the data files
#' 											to upload. If not given will occur in a temp dir
#'
#' @author karl
#' @export
run_tm_etl <- function(
  data_dfs,
  mapping_df,
  host = 'localhost',
  port = 5432,
  java = 'java',
  jar = find_extdata_file('tm_etl.jar'),
  config = find_extdata_file('Config.groovy.brew'),
  dir = NULL
) {
  check_jar(java, jar)

  if (is.null(dir)) {
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  }

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  ## config
  config_file <- 'Config.groovy'
  brew::brew




}


build_tmdataloader_mapping_file <- function() {

}




check_jar <- function(java, jar) {
  args <- sprintf('-jar %s -h', jar)
  out <- system2(java, args, stdout = TRUE)
  grepl('usage', out[1])
}
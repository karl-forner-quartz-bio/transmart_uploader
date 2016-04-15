

#' run the tMDataLoader tool
#'
#' @param platform	the platform for probe id mapping to the gene symbol
#' @param data_dfs		the expression data to upload
#' @param map_df		the corresponding mapping
#' @param etl_path		the path in etl tree, as a standard path string
#' @param host			the transmart DB host
#' @param port			the transmart DB port
#' @param java			the java executable to use
#' @param jar			the tMDataLoader jar file
#' @param config		the tMDataLoader config file template
#' @param dir			the directory in which to setup the data files
#' 						to upload. If not given will occur in a temp dir
#' @param ...			additional arguments to \code{execute_etl_cmd}
#'
#' @return the upload summary statistics as a data frame, or NULL
#'
#' @author Sepideh
#' @export
run_tm_etl_for_expression_data <- function(
  platform,
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
  study_id <- unique(map_df$STUDY_ID)
  if (is.null(study_id) || !nzchar(study_id)) {
    stop('STUDY_ID is MANDATORY')
  }
  
  platform_name <- unique(map_df$PLATFORM)
  
  if (is.null(dir)) {
    dir <- tempfile()
	#dir <- "/home/sbabaei/kidney"
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

  setup_etl_files_for_expression(platform=platform, data_dfs, map_df, data_dir, etl_path, study_id, platform_name)

  out <- execute_etl_cmd(java, jar, config_file, ...)

  #stats <- fetch_etl_summary_statistics(file.path(data_dir, etl_path))
  #invisible(stats)
}


setup_etl_files_for_expression <- function(platform, data_dfs, map_df, data_dir, etl_path, prefix, platform_name) {

  # make transmart path
  path <- file.path(data_dir, etl_path)
  dir.create(path, recursive = TRUE)

  map_path <- file.path(path, paste0(prefix, '_Gene_Expression_Data_L.txt'))
  write.table(data_dfs, map_path, sep = '\t', quote = FALSE, row.names = FALSE, eol = "\r\n")

 map_path <- file.path(path, paste0(prefix, '_Subject_Sample_Mapping_File.txt'))
 write.table(map_df, map_path, sep = '\t', quote = FALSE, row.names = FALSE, eol = "\r\n")

 map_path <- file.path(path, paste0(platform_name,'.txt'))
 writeLines(platform, map_path)
}



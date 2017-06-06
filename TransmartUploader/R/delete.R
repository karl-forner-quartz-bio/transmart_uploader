### functions for deleting stuff from Transamrt



#' delete a study by id using the tMDataLoader tool
#'
#' @param id								the id of the study to delete
#' @param ... 		  			  passed to run_etl_command
#'
#' @return the command output
#'
#' @author karl
#' @export
delete_study_by_id <- function(id, ...) {
  cmd <- paste('--delete-study-by-id', toupper(id))
  invisible(run_etl_command(extra = cmd, ...))
}


#' delete a study by path using the tMDataLoader tool
#'
#' @param path							the transmart path of the study
#' @param ... 		  			  passed to run_etl_command
#'
#' @return the command output
#'
#' @author karl
#' @export
delete_study_by_path <- function(path, ...) {
  path <- paste0('\\\\', gsub('/', '\\\\\\\\', path))
  cmd <- paste('--delete-study-by-path', path)
  invisible(run_etl_command(extra = cmd, ...))
}


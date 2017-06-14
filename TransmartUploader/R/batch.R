

new_upload_job <- function(df, mapping) {
  list(data = df, mapping = mapping)
}

#' create a simple upload job
#'
#' @param study_id		the study ID
#' @param ...					passed to \code{\link{build_mapping_file}}
#' @inheritParams mapping
#' @inheritParams simple_categorization
#'
#' @return 				data frame of Luminex values and mapping table
#'
#' @author karl
#' @export
simple_upload_job <- function(df, study_id,
  category,
  mapping = build_mapping_file(df, categ, ...),
  categ = simple_categorization(df, category),
  ...)
{
  df <- format_input_data(df, study_id)
  new_upload_job(df, mapping)
}

#' make an upload job from a list of data frames and categories
#'
#' upload a named list of data frames in one shot:
#' \itemize{
#' 		\item all data frames must have mandatory vars SUBJ_ID and VISIT_NAME
#' 		\item only unique variables beween data frames are kept, except for the
#' 				mandatory vars, and the vars to \code{keep}
#' 		\item vars are categorized by data frame, i.e.  vars from a data frame
#' 					are all put in the same category
#' 		\item	using the \code{keep} param, you can keep some vars to be uploaded
#' 					by deciding in which data frame the keep them
#' 		\item eventually all data are merged in one single data frame
#' }
#'
#'
#' @param	data_dfs		a named list of data frames. The names are only used
#' 	as default for \var{categories} and for the \var{keep} param.
#' @param study_id		the study ID
#' @param categories	the transmart category to use for each data frame, as a
#' 	character vector of same length than \var{data_dfs}
#' @param keep				a named list of character vectors (using data_dfs names)
#' 		specifying for some data frames which variables to keep, in case they
#' 		are non-unique. So if you have a variable present in several data frames
#' 		you can specify which data frame should keep it, and hence in which category
#' 		it should end up.
#'
#' @return the upload job
#'
#' @author karl
#' @export
#' @export
multiple_upload_job <- function(
  data_dfs,
  study_id,
  categories = names(data_dfs),
  keep = NULL)
{
  mandatory_vars <-  c('SUBJ_ID', 'VISIT_NAME')

  ### fetch list of duplicated columns across tables
  all_vars <- unlist(lapply(data_dfs, colnames), use.names = FALSE)
  duplicated_vars <- unique(all_vars[duplicated(all_vars)])

  ### remove all duplicated columns and predefined useless columns
  to_remove <- setdiff(duplicated_vars, mandatory_vars)
  .remove_vars <- function(df, vars)  df[setdiff(names(df), vars)]
  tbls <- sapply(data_dfs, .remove_vars, to_remove, simplify = FALSE)

  ### restore the columns to keep
  if (!is.null(keep)) {
    for (nm in names(keep)) {
      cols <- keep[[nm]]
      tbls[[nm]][cols] <- data_dfs[[nm]][cols]
    }
  }

  ### hopefully no more duplicated vars ==> merge straightforward
  .merge <- function(x, y)  merge(x, y, by = mandatory_vars, all = TRUE)
  df <- Reduce(.merge, tbls)

  df <- format_input_data(df, study_id)
  categ <- multi_categorization(tbls, categories)
  mapping <- build_mapping_file(df, categ)

  new_upload_job(df, mapping)
}


#' upload a list of jobs at once
#'
#' @inheritParams mapping
#' @param jobs	the list of jobs to upload
#' @param ...		passed to \code{\link{upload_low_dimensional_data}}
#'
#' @author karl
#' @export
#' @seealso simple_upload_job
#' @seealso multiple_upload_job
batch_upload_low_dimensional_data <- function(jobs, merge = 'REPLACE',...) {

  # mappings
  mappings <- lapply(jobs, getElement, 'mapping')

  # reset the filenames
  filenames <- paste0('data', seq_along(jobs), '.txt')

  for (i in seq_along(mappings))
    mappings[[i]]$filename <- filenames[i]

  # make a unique mapping file
  mapping <- do.call(rbind, mappings)
  rownames(mapping) <- NULL
  attr(mapping, 'merge') <- merge

  data_dfs <- lapply(jobs, getElement, 'data')

  upload_low_dimensional_data(data_dfs, mapping = mapping, ...)
}




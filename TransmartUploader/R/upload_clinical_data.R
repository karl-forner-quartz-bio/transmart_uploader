#' bulk upload clinical data tables
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
#' 		\item eventually all data are merged in one single data frame, which is
#' 				then uploaded, which speeds up the process
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
#' @param ...				additional arguments to \code{\link{upload_clinical_data}}
#'
#' @return the \code{\link{upload_clinical_data}} output
#'
#' @author karl
#' @export
bulk_upload_clinical_data <- function(
  data_dfs,
  study_id,
  categories = names(data_dfs),
  keep = NULL,
  ...
) {

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

  upload_clinical_data(df, categ = categ, ...)
}

#' categorization of multiple tables
#'
#' @inheritParams mapping
#' @param default_category	a default category to apply to \code{vars} if not null
#' @param vars							the vars on which to apply the default category
#' @return a data.frame with columns "category_cd" and "data_label"
#'
#' @seealso mapping
#' @author karl
#' @keywords internal
multi_categorization <- function(
  data_dfs,
  categories,
  categ = base_categorization())
{
  base = base_categorization()
  null_categ <- base[NULL, ]

  categs <- mapply(simple_categorization, data_dfs, categories,
    MoreArgs = list(categ = null_categ), SIMPLIFY = FALSE)
  categ <- do.call(rbind.data.frame, categs)
  row.names(categ) <- NULL

  rbind(base, categ, stringsAsFactors = FALSE)
}


#' upload clinical data (no high dimensional data) using tMDataLoader
#'
#' @inheritParams simple_categorization
#' @inheritParams mapping
#' @inheritParams params
#' @param ...				additional arguments to \code{execute_etl_cmd}
#'
#' @return the \code{\link{run_tm_etl_on_processed_data}} output
#'
#' @author karl
#' @export
upload_clinical_data <- function(
  df,
  etl_path,
  default_category = NULL,
  mapping = build_mapping_file(df, categ, merge = merge),
  categ = simple_categorization(df, default_category),
  merge = 'REPLACE',
  ...
) {

  etl_path <- paste0(etl_path, "/ClinicalData")

  res <- run_tm_etl_on_processed_data(df, mapping, etl_path = etl_path, ...)

  res
}


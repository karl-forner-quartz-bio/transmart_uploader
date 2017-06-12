#' bulk upload clinical data tables
#'
#' fast upload of multiple tables/data frame
#'
#' will remove duplicate columns/vars
#'
#' @inheritParams simple_categorization
#' @inheritParams build_mapping_file
#' @param tissue_type		tissue_type of data
#' @param transmart_path	the path in etl tree, as a standard path string
#' @param ...				additional arguments to \code{execute_etl_cmd}
#'
#' @return the \code{\link{run_tm_etl_on_processed_data}} output
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

  upload_clinical_data(df, etl_path, categ = categ, ...)
}



# NB: repeated columns are
merge_data_dfs <- function(data_dfs, by, suffixes = names(data_dfs)) {
  suffixes <- paste0('..', suffixes)

  # trick: make a list of length-1 named lists
  .add_suffix <- function(df, nm) { attr(df, 'suffix') <- nm; df }
  tbls <- mapply(.add_suffix, data_dfs, suffixes, SIMPLIFY = FALSE)

  .merge <- function(x, y) {
    z <- merge(x, y, by = by, all = TRUE,
      suffixes = c(attr(x, 'suffix'), attr(y, 'suffix')))
    attr(z, 'suffix') <- attr(y, 'suffix') # N.B: this is arbitrary
    z
  }
  df <- Reduce(.merge, tbls)
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
#' @inheritParams build_mapping_file
#' @param tissue_type		tissue_type of data
#' @param transmart_path	the path in etl tree, as a standard path string
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


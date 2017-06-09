
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


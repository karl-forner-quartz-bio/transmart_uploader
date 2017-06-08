
#' upload clinical data (no high dimensional data) using tMDataLoader
#'
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
  mapping = build_mapping_file(df, categ),
  categ = base_categorization(),
  ...
) {

  etl_path <- paste0(etl_path, "/ClinicalData")

  res <- run_tm_etl_on_processed_data(df, mapping, etl_path = etl_path, ...)

  res
}



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
upload_low_dimensional_data <- function(
  df,
  etl_path,
  category = NULL,
  mapping = build_mapping_file(df, categ, merge = merge),
  categ = simple_categorization(df, category),
  merge = 'REPLACE',
  ...
) {

  etl_path <- paste0(etl_path, "/ClinicalData")

  res <- run_tm_etl_on_processed_data(df, mapping, etl_path = etl_path, ...)

  res
}


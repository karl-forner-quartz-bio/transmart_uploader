
#' upload gene expression data (high dimensional data) using tMDataLoader
#'
#' @param dfs								the data to upload as a data frame or a list
#'                          of data frames
#' @param study_id					the transmart STUDYID
#' @param mapping						the corresponding mapping
#' @param tissue_type				tissue_type of data
#' @param transmart_path		the path in etl tree, as a standard path string
#' @param ...								additional arguments to \code{execute_etl_cmd}
#'
#' @return the upload summary statistics as a data frame, or NULL
#'
#' @author Sepideh
#' @export
upload_gene_expression_data <- function(
  platform,
  df,
  study_id,
  map_df,
  tissue_type = " ",
  transmart_path = paste0("Tests", "_", study_id),
  ...
) {
 
  #add "...ToUpload" to the path
  transmart_path = paste0(transmart_path, "/ExpressionDataToUpload")
 
  res <- run_tm_etl_for_expression_data(platform = platform, data_dfs= df, map_df = map_df, etl_path = transmart_path, dir="/home/sbabaei/kidney")

  res
}










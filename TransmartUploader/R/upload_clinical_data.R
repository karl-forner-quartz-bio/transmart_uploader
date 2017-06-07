
#' upload clinical data (no high dimensional data) using tMDataLoader
#'
#' @param dfs				the data to upload as a data frame or a list
#'                          of data frames
#' @param study_id			the transmart STUDYID
#' @param mapping			the corresponding mapping
#' @param tissue_type		tissue_type of data
#' @param transmart_path	the path in etl tree, as a standard path string
#' @param ...				additional arguments to \code{execute_etl_cmd}
#'
#' @return the upload summary statistics as a data frame, or NULL
#'
#' @author Sepideh
#' @export
upload_clinical_data <- function(
  dfs,
  study_id,
  mapping = default_mapper(),
  tissue_type = " ",
  transmart_path = paste0("Tests", "_", study_id),
  ...
) {
  if (inherits(dfs, 'data.frame')) dfs <- list(dfs)

  #add "...ToUpload" to the path
  transmart_path = paste0(transmart_path, "/ClinicalDataToUpload")


  .fix_df <- function(df) {
    if (!is.null(df$Tissue_Type)) df$Tissue_Type <- tissue_type
    #Add STUDY_ID as a column to the data frames
    df$STUDY_ID <- study_id
    df
  }

  dfs <- lapply(dfs, .fix_df)

  res <- run_tm_etl(dfs, mapping, etl_path = transmart_path, ...)

  res
}


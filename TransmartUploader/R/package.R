#' TransmartUploader R package
#'
#' A R package that interfaces the **tMdataloader** (https://github.com/ThomsonReuters-LSPS/tMDataLoader)
#' ETL tool to upload data into **transmart**.
#' Specifically developed for the www.precisesads.eu project.
#'
#' The current version of **tMdataloader** is 1.2.4-116.
#'
#'
#' The main functions are, from high level to lower level:
#'\itemize{
#'   \item{\code{\link{batch_upload_low_dimensional_data}}}
#'   \item{\code{\link{multiple_upload_job}}}
#'   \item{\code{\link{simple_upload_job}}}
#'   \item{\code{\link{upload_low_dimensional_data}}}
#'   \item{\code{\link{run_tm_etl_on_processed_data}}}
#'   \item{\code{\link{delete_study_by_id}}}
#'   \item{\code{\link{delete_study_by_path}}}
#'   \item{\code{\link{simple_categorization}}}
#'   \item{\code{\link{format_input_data}}}
#'}
#'
#' @name TransmartUploader-package
#' @aliases TransmartUploader
#' @docType package
#' @title TransmartUploader package
#' @author Quartz Bio
#' @keywords package
#'
NULL

#' format input data for tMDataLoader
#'
#' - add if needed the STUDY_ID column
#' - check that SUBJ_ID is present, or die
#' - reorder columns so that STUDY_ID and SUBJ_ID are first
#'
#' @param df					a data frame to format before uploading to Transmart
#' @param study_id		the study_id to set or replace if not null
#' @param tissue_type	the Tissue_Type to set or replace if not null
#' @inheritParams params
#'
#' @return the formatted data frame
#' @author karl
#' @export
format_input_data <- function(df, study_id = NULL, tissue_type = NULL) {
  if (!is.null(study_id)) df$STUDY_ID <- study_id
  if (!is.null(tissue_type)) df$Tissue_Type <- tissue_type

  ndf <- names(df)

  if (!'STUDY_ID' %in% names(df)) stop("STUDY_ID column is missing")
  if (!'SUBJ_ID' %in% names(df)) stop("SUBJ_ID column is missing")

  # reorder the columns
  mandatory <- c('STUDY_ID', 'SUBJ_ID')
  if (!is.null(df$Tissue_Type)) mandatory <- c(mandatory, 'Tissue_Type')

  newcols <- c(mandatory, setdiff(names(df), mandatory))
  df <- df[, newcols, drop = FALSE]

  df
}


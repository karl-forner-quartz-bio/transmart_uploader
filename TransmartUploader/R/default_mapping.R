
#' Generate the default mapping of variables to the TranSMART tree
#'
#' this mapping is mandatory to upload the data into tranSMART
#' This is the default map and should be edited based on our tranSMARt tree structure
#'
#' @return map_data   data.frame with columns "category_cd" and "data_label"
#'
#' @author Sepideh
#' @export
default_mapping <- function() {

  col_path <- " / /Tissue/Subject+Demographic/Subject+Phenotype"
  col_name <- "STUDY_ID/SUBJ_ID/Tissue_Type/Sex/Phenotype"

  col_path <- unlist(strsplit(col_path, "/"))
  col_name <- unlist(strsplit(col_name, "/"))

  map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
  names(map_data) <- c("category_cd", "data_label")


  map_data
}

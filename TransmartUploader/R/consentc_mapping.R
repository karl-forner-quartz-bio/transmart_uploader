#' Generate the mapping of variables to the TranSMART tree for demography data
#'
#' this mapping is mandatory to upload the data into tranSMART
#' @ study_type: "Inception" or "Cross_sectional"
#' study_type = "Cross_sectional"
#' @return map_data   data.frame with columns "category_cd" and "data_label"
#'
#' @author Sepideh
#' @export
consentc_mapping <- function(study_type) {

path0 = "Study_phase+"
col_path <-"CS_phase/
PatientID"
col_path <- unlist(strsplit(col_path, "/\n"))
col_name <- col_path
## col_name <- gsub("_","", col_name)

col_path<- paste0(path0, col_path)
col_path <- c(" ", " ", col_path)
col_name <- c("STUDY_ID","SUBJ_ID",col_name )
 map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
 names(map_data) <- c("category_cd", "data_label")

  map_data
}

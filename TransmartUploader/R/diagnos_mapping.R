
#' Generate the mapping of variables to the TranSMART tree fo r diagnos data
#'
#' this mapping is mandatory to upload the data into tranSMART
#' @ study_type: "Inception" or "Cross_sectional"
#' study_type = "Cross_sectional"
#' @return map_data   data.frame with columns "category_cd" and "data_label"
#'
#' @author Sepideh
#' @export
diagnos_mapping <- function(study_type) {

path0 = paste0(study_type, "+Clinical_data+")
col_path <-"Entry_data+Center_ID/
Entry_data+Entry_ID/
Entry_data+Flag/
Entry_data+Record_ID/
Entry_data+Entry_date/
Entry_data+Status/
Entry_data+Page_NO/
Entry_data+Last_modification_date/
Entry_data+Country_ISO_code/
Entry_data+Repeat_page_number/
Diagnos+Disease/
Diagnos+Onset_Date/
Diagnos+Onset_Day/
Diagnos+Onset_Month/
Diagnos+Onset_Year/
Diagnos+Onset_Date_digit/
Diagnos+Visit/
Diagnos+PBLANK/
Diagnos+Repeat_visit_number/
Subject+Patient_ID/
Subject+Phenotype"
col_path <- unlist(strsplit(col_path, "/\n"))
col_name <- matrix(unlist(strsplit(col_path, "\\+")), ncol = 2 , byrow = TRUE)
col_name <- col_name[,2]

col_path<- paste0(path0, col_path)
col_path <- c(" ", " ", col_path)
col_name <- c("STUDY_ID","SUBJ_ID",col_name )
 map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
 names(map_data) <- c("category_cd", "data_label")

if (study_type == "Inception") map_data <- map_data[-grep("Phenotype", map_data[,2]),]
  map_data
}


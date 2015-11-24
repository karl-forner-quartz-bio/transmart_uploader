
#' Generate the mapping of variables to the TranSMART tree fo r diagnos data
#'
#' this mapping is mandatory to upload the data into tranSMART
#' @param study_type: "Inception" or "Cross_sectional"
#' @return map_data   data.frame with columns "category_cd" and "data_label"
#'
#' @author Sepideh
#' @export
diagnos_mapping <- function(study_type) {

# path0 = paste0(study_type, "+Clinical_data+")
col_path <-"Entry_data+CenterID/
Entry_data+EntryID/
Entry_data+Flag/
Entry_data+RecordID/
Entry_data+EntryDate/
Entry_data+Status/
Entry_data+PageNO/
Entry_data+Lastmodificationdate/
Entry_data+CountryISOcode/
Entry_data+Repeatpagnumber/
Diagnosis+Disease/
Diagnosis+OnsetDate/
Diagnosis+OnsetDay/
Diagnosis+OnsetMonth/
Diagnosis+OnsetYear/
Diagnosis+OnsetDateDigit/
Diagnosis+Visit/
Diagnosis+PBLANK/
Diagnosis+Repeatvisitnumber/
Subject+PatientID/
Subject+Phenotype"
col_path <- unlist(strsplit(col_path, "/\n"))
col_name <- matrix(unlist(strsplit(col_path, "\\+")), ncol = 2 , byrow = TRUE)
col_name <- col_name[,2]

# col_path<- paste0(path0, col_path)
col_path <- c(" ", " ", col_path)
col_name <- c("STUDY_ID","SUBJ_ID",col_name )
 map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
 names(map_data) <- c("category_cd", "data_label")

if (study_type == "Inception") map_data <- map_data[-grep("Phenotype", map_data[,2]),]
  map_data
}


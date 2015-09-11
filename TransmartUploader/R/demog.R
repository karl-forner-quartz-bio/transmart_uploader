#' upload a demog.txt file file into transmart
#'
#'   demog.sas7bdat consist of demographic information of patients,
#'  .sas7bdat files can be read and converted to txt files in R using "haven"
#'
#' @param path				the demog.txt file path
#' @inheritParams 			upload_clinical_data
#' @return sample_data		the data.frame with X cols, fixed col names: "STUDY_ID", "SUBJ_ID",...
#'
#' @author Sepideh
#' @export
#' @CRF demog
upload_demog <- function(path, transmart_path, study_type, ...) {
  
  raw <- read_demog(path)
  demog <- format_demog(raw)  
  study_id <- unique(raw$STUD_ID)
  study_id <- paste0(study_id,"_demog")
  
  upload_clinical_data(demog, study_id, transmart_path = transmart_path, mapping = demog_mapping(study_type))
}


#' read a demog file
#'
#' @param path		the demog file path
#' @return the file content as a data frame
#' @author Sepideh
#' @export
read_demog <- function(path) {
  read.table(path, header = T, check.names = FALSE, stringsAsFactors = FALSE, sep = "\t")
}


#' curate demog data to upload to tranSMART
#'
#' @param demog_data		the data.frame read from a "demog.txt" ("demog.sas7bdat") file
#' @return sample_data		the data.frame with X cols, fixed col names: "SUBJ_ID", "Sex", "Race", "Age" ...
#' @author Sepideh
#' @export

format_demog <- function(demog_data) {

if (length(unique(demog_data$PATID_ID)) == 1) stop("There is no SUBJECT ID")

if (anyDuplicated(demog_data$PATID_ID) != 0) stop("Error: duplicated Patient ID")

sex   <- recode(demog_data$SEX, "1='Male'; 2='Female';else=NA")
race <- recode(demog_data$RACE, "1='Caucasian/White'; 2='Black/African American'; 3='Asian'; 4='American Indian/Alaska native'; 5=' Native Hawaiian/ Other Pacific Islander'; 6='Other';else=NA")

res <- data.frame(SUBJ_ID = demog_data$PATID_ID, 
				Sex 		= sex,
				Race		= race,
				Age 		= demog_data$AGE_D,
				Age_unit 	= demog_data$AGE_DU,
				Birthday 	= demog_data$BIRTHDAT,
				Birth_year 	= demog_data$YBIRTH,
				Page_NO 	= demog_data$PAGENO,
				Last_modification_date  = demog_data$MERGE_DA,
				Repeat_page_number 	= demog_data$PAGEREP,
				Patient_ID 		= demog_data$PATIDENT,
                stringsAsFactors = FALSE)
				  
  res
}

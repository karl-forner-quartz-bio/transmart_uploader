#' upload a consentc.txt file file into transmart
#'
#'   consentc.sas7bdat consist of consent information of patients,
#'  .sas7bdat files can be read and converted to txt files in R using "haven"
#'
#' @param path				the consentc.txt file path
#' @inheritParams 			upload_clinical_data
#' @return sample_data		the data.frame with X cols, fixed col names: "STUDY_ID", "SUBJ_ID",...
#'
#' @author Sepideh
#' @export
#' @family consentc
upload_consentc <- function(path, transmart_path, study_type, ...) {

  raw <- read_consentc(path)
  consentc <- format_consentc(raw)
  study_id <- unique(raw$STUD_ID)
  study_id <- paste0(study_id,"_consentc")

  upload_clinical_data(consentc, study_id, transmart_path = transmart_path, mapping = consentc_mapping(study_type))
}


#' read a consentc file
#'
#' @param path		the consentc file path
#' @return the file content as a data frame
#' @author Sepideh
#' @export
read_consentc <- function(path) {
  read.table(path, header = T, check.names = FALSE, stringsAsFactors = FALSE, sep = "\t")
}


#' curate consentc data to upload to tranSMART
#'
#' @param consentc_data		the data.frame read from a "consentc.txt" ("consentc.sas7bdat") file
#' @return sample_data		the data.frame with X cols, fixed col names: "SUBJ_ID", "phase" ...
#' @author Sepideh
#' @export

format_consentc <- function(consentc_data) {

if (length(unique(consentc_data$PATID_ID)) == 1) stop("There is no SUBJECT ID")

if (anyDuplicated(consentc_data$PATID_ID) != 0) stop("Error: duplicated Patient ID")

phase   <- recode(consentc_data$BIOYN, "1='I'; 0='II';else=NA")

res <- data.frame(SUBJ_ID = consentc_data$PATID_ID,
				CS_phase 	= phase,
				PatientID 		= consentc_data$PATIDENT,
                stringsAsFactors = FALSE)

# res[res==""] <- NA
res[is.na(res)] <- ""

  res
}

#' upload a sampling.txt file file into transmart
#'
#'   sampling.sas7bdat consist of samplingraphic information of patients,
#'  .sas7bdat files can be read and converted to txt files in R using "haven"
#'
#' @param path				the sampling.txt file path
#' @inheritParams 			upload_clinical_data
#' @return sample_data		the data.frame with X cols, fixed col names: "STUDY_ID", "SUBJ_ID",...
#'
#' @author Sepideh
#' @export
#' @family sampling
upload_sampling <- function(path, transmart_path, study_type, ...) {

  raw <- read_sampling(path)
  sampling <- format_sampling(raw)
  study_id <- unique(raw$STUD_ID)
  study_id <- paste0(study_id,"_sampling")

  upload_clinical_data(sampling, study_id, transmart_path = transmart_path, mapping = sampling_mapping(study_type))
}


#' read a sampling file
#'
#' @param path		the sampling file path
#' @return the file content as a data frame
#' @author Sepideh
#' @export
read_sampling <- function(path) {
  read.table(path, header = T, check.names = FALSE, stringsAsFactors = FALSE, sep = "\t")
}


#' curate sampling data to upload to tranSMART
#'
#' @param sampling_data		the data.frame read from a "sampling.txt" ("sampling.sas7bdat") file
#' @return sample_data		the data.frame with X cols, fixed col names: "SUBJ_ID", "Sex", "Race", "Age" ...
#' @author Sepideh
#' @export

format_sampling <- function(sampling_data) {

if (length(unique(sampling_data$PATID_ID)) == 1) stop("There is no SUBJECT ID")

if (anyDuplicated(sampling_data$PATID_ID) != 0) stop("Error: duplicated Patient ID")

omic <- as.character(sampling_data$ITEM001)
res <- data.frame(SUBJ_ID = sampling_data$PATID_ID,
				PageNOS 			= sampling_data$PAGENO,
				DateOfSamplingD 	= sampling_data$DATE1DAT,
				DateOfSampling		= sampling_data$DATE1DATC,
				OMICnumber 			= omic,
				SampleRecordID 		= sampling_data$CT_RECID,
				LastModificationDateS  = sampling_data$MERGE_DA,
				RepeatPageNumberS	= sampling_data$PAGEREP,
				PatientID 			= sampling_data$PATIDENT,
                stringsAsFactors 	= FALSE)

# res[res==""] <- NA
res[is.na(res)] <- ""

  res
}

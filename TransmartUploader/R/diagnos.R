#' upload a Diagnos.tsv (or SAS) file into transmart
#'
#' Diagnos.tsv (or sas7bdat) consist of diagnostic information of patients,
#'  .sas7bdat files can be read and converted to txt files in R using "haven"
#'
#' @param path		the diagnos.tsv file path
#' @inheritParams upload_clinical_data
#' @return sample_data		the data.frame with X cols, fixed col names: "STUDY_ID", "SUBJ_ID",...
#'
#' @author Sepideh
#' @export
#' @CRF Diagnos
upload_diagnos <- function(path, transmart_path, study_type, ...) {
  
  raw <- read_diagnos(path)
  diagnos  <- format_diagnos(raw)  
  study_id <- unique(raw$STUD_ID)
  study_id <- paste0(study_id,"_diagnos")
  
  upload_clinical_data(diagnos, study_id, transmart_path = transmart_path, mapping = diagnos_mapping(study_type))
}


#' read a Diagnos file
#'
#' @param path		the Diagnos file path
#' @return the file content as a data frame
#' @author Sepideh
#' @export
read_diagnos <- function(path) {
  read.table(path, header = T, check.names = FALSE, stringsAsFactors = FALSE, sep = "\t")
}


#' curate Diagnos data to upload to tranSMART
#'
#' @param diagnos_data				the data.frame read from a "diagnos.sas7bdat" file
#' @return sample_data		the data.frame with X cols, fixed col names: "SUBJ_ID", "Phenotype"...
#' @author Sepideh
#' @export

format_diagnos <- function(diagnos_data) {

if (length(unique(diagnos_data$PATID_ID)) == 1) stop("There is no SUBJECT ID")

if (anyDuplicated(diagnos_data$PATID_ID) != 0) stop("Error: duplicated Patient ID")

pheno   <- recode(diagnos_data$YESNO1, "1='Control'; 0='Case';else=NA")
disease <- recode(diagnos_data$ITEM001, "0='RA'; 1='SLE'; 2='SSc'; 3='SjS'; 4='MCTD'; 5='PAPs'; 6='UCTD'; else=NA")

res <- data.frame(SUBJ_ID 		= diagnos_data$PATID_ID, 
		Phenotype 		= pheno, 
		Disease			= disease, 
                Onset_Date		= diagnos_data$DATE1DATC,
		Onset_Day 		= diagnos_data$DDATE1,
		Onset_Month		= diagnos_data$MDATE1,
		Onset_Year 		= diagnos_data$YDATE1,
		Onset_Date_digit	= diagnos_data$DATE1DAT,
                Visit     		= diagnos_data$VISIT,
                Patient_ID 		= diagnos_data$PATIDENT,
                Center_ID  		= diagnos_data$CENT_ID,
                Entry_ID   		= diagnos_data$ENTRY_ID,
		Flag 			= diagnos_data$FLAG_,
		Record_ID 		= diagnos_data$CT_RECID,
		Entry_date		= diagnos_data$ENTRY_DA,
		Status			= diagnos_data$STATUS,
		Page_NO 		= diagnos_data$PAGENO,
		PBLANK			= diagnos_data$PBLANK_D,
		Last_modification_date  = diagnos_data$MERGE_DA,
		Country_ISO_code 	= diagnos_data$COUN_ID,
		Repeat_visit_number 	= diagnos_data$VISITREP,	
		Repeat_page_number 	= diagnos_data$PAGEREP,
                stringsAsFactors = FALSE)
				  
  res
}
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
center <-  recode(diagnos_data$CENT_ID, "1001='1001_ES_SAS_Malaga'; 
1002='1002_ES_SAS_Granada_ER';
1003='1003_ES_SAS_Granada_NO';
1004='1004_ES_SAS_Cordoba';
1005='1005_FRA_UBO'; 
1006='1006_ITA_IRCCS';
1007='1007_GER_DRFZ';
1008='1008_CHE_UNIGE'; 
1009='1009_PRT_CHP';
1010='1010_GER_MHH';
1011='1011_BEL_KU LEUVEN';
1012='1012_BEL_UCL'; 
1013='1013_ITA_UNIMI'; 
1014='1014_GER_UKK'; 
1015='1015_AUT_MUW'; 
1016='1016_HUN_USZ'; 
1017='1017_ESP_SCS'; 
1018='1018_ESP_IDIBAPS'; 
1019='1019_ESP_Biobank'")
country <- as.character(diagnos_data$COUN_ID)

res <- data.frame(SUBJ_ID 		= diagnos_data$PATID_ID, 
		DiseaseStatus 	= pheno, 
		Disease			= disease, 
        OnsetDate		= diagnos_data$DATE1DATC,
		OnsetDay 		= diagnos_data$DDATE1,
		OnsetMonth		= diagnos_data$MDATE1,
		OnsetYear 		= diagnos_data$YDATE1,
		OnsetDateDigit	= diagnos_data$DATE1DAT,
        Visit     		= diagnos_data$VISIT,
        PatientID 		= diagnos_data$PATIDENT,
        CenterID  		= center,
        EntryID   		= diagnos_data$ENTRY_ID,
		Flag 			= diagnos_data$FLAG_,
		RecordID 		= diagnos_data$CT_RECID,
		EntryDate		= diagnos_data$ENTRY_DA,
		Status			= diagnos_data$STATUS,
		PageNO 			= diagnos_data$PAGENO,
		PBLANK			= diagnos_data$PBLANK_D,
		LastModificationDate  = diagnos_data$MERGE_DA,
		CountryISOcode 	= country,
		RepeatVisitNumber 	= diagnos_data$VISITREP,	
		RepeatPageNumber 	= diagnos_data$PAGEREP,
        stringsAsFactors = FALSE)
	
res[is.na(res)] <- ""
	
  res
}





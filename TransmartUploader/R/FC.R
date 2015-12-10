#' upload a Felow Cytometry processed .csv file file into transmart
#'
#'
#' @param path				Folder path including all FC csv files for Cs or Inception
#' @inheritParams 			upload_clinical_data
#' @return sample_data		the data.frame with X cols, fixed col names: "STUDY_ID", "SUBJ_ID",...
#'
#' @author Sepideh
#' @export
#' @FC csv 
upload_FC <- function(path, OMICpath, transmart_path, study_type, ...) {
  
  raw <- read_FCs(path, study_type)
  OMIC <-read_OMIC(OMICpath)  
  
  FCs <- format_FCs(raw, OMIC) 
  map <- FCs_mapping (FCs)
  if (study_type == "Inception") study_id <- "AD2"
  if (study_type == "Cross_Sectional") study_id <- "AD1"
  
  upload_clinical_data(FCs, study_id, transmart_path = transmart_path, mapping = map)
}


#' read FCs files
#'
#' @param path		the FCs directory path
#' @return the file FC as a data frame
#' @author Sepideh
#' @export
read_FCs <- function(path, study_type) {
 
files <- list.files(path, pattern = "\\.csv$")

dfs <- lapply( paste0(path,files), read.delim, header = T, check.names = FALSE, stringsAsFactors = FALSE)

format_df <- function(df) {  
  df$comb <- paste(df[[1]],df[[2]], sep = "__")  
  df
}
dfs <- lapply(dfs, format_df)

FCs <- Reduce(function(x, y) merge(x, y, by ="comb", all=TRUE), dfs)
vars <- names(FCs)

if (study_type == "Cross_Sectional") {
	ids <-vars[grep("PHASE", vars)]
	n=4
	}
if (study_type == "Inception") {
 	 ids <-vars[grep("Inception", vars)]
	n=3
	}
ids <- matrix(unlist(strsplit(ids, "_")), ncol= n, byrow=TRUE)

Abs   <- cbind(paste0(FCs$comb,"__Absolute_number"), FCs[,grep("Absolute", vars)])
names(Abs) <-"N"
Freq  <- cbind(paste0(FCs$comb,"__Frequency"),FCs[,grep("Frequency", vars)])
names(Freq) <-"N"
MFI   <- cbind(paste0(FCs$comb,"__MFI"),FCs[,grep("MFI", vars)])
names(MFI) <-"N"
Ratio <- cbind(paste0(FCs$comb,"__Ratio"),FCs[,grep("Ratio", vars)])
names(Ratio) <-"N"

##FCs <- rbindlist(list(Abs, Freq, MFI, Ratio))
FCs <- rbind(Abs, Freq, MFI, Ratio)

list(FCs, ids)
}


#' curate FCs data to upload to tranSMART
#'
#' @param FCs		the data.frame read from a FCs files 
#' @return sample_data		the data.frame with X cols, fixed col names: "SUBJ_ID", ...
#' @author Sepideh
#' @export

format_FCs <- function(raw, OMIC) {

data <- raw[[1]]
data [[1]] <- gsub("-","_neg ", data [[1]])
data [[1]] <- gsub("\\+","_pos ", data [[1]])
data  <- t(data )
colnames (data )<- data [1,]
data  <- data [-1,]

##some check for number of panels...
ids <- raw[[2]]
res <- data.frame(cbind(SUBJ_ID= OMIC[match(ids[,1], OMIC[,2]),1], DateFC= OMIC[match(ids[,1], OMIC[,2]),2], data), stringsAsFactors = FALSE )			
# res[res==""] <- NA
res[is.na(res)] <- ""
				  
  res
}

#' read FCs files
#'
#' @param path		the OMIC number mapping to PatientID path
#' @return the file OMIC as a data frame
#' @author Sepideh
#' @export
read_OMIC <- function(OMICpath) {
 
OMIC <- read.delim(OMICpath)
}

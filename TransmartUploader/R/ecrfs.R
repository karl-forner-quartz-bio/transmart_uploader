#' Read clinical data from the db_clinical to upload in transmart
#'
#' @param DB				the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/db_clinical.db"), PRECISESADS db_clincial consists of ALL ecrfs of patients with unique patient ids
#' @param study_type		"cross_sectional" or "inception"
#'
#' @return 					df of all clinical data sorted by SUBJ_ID and VISIT, study id, crf labels
#'
#' @author Sepideh
#'
#' @export

read_db <- function(DB, study_type, ...) {

 db = dbConnect(RSQLite::SQLite(), DB)
 dblist <- dbListTables(db)
 dblist_sub <- subset(dblist, grepl(study_type, dblist))

 dblist_sub <- subset(dblist_sub, !grepl("mapping", dblist_sub))
 dblist_sub <- subset(dblist_sub, !grepl("_flag", dblist_sub))

 crf_label <- 0
 df <- dbReadTable(db,dblist_sub[1])
 study_id <- unique(df$STUDY_ID)

 crf <- gsub(paste0(study_type,"_"),"", dblist_sub)
 crf<- paste0(".", crf)
 crf_label[1:ncol(df)] <- crf[1]

 for (cnt in 2:length(dblist_sub))
 {
 tmp <- dbReadTable(db, dblist_sub[cnt])
 l1 <- ncol(df)

df <- merge(df, tmp, by= c("SUBJ_ID", "VISIT"), suffixes = c(crf[cnt-1], crf[cnt]), all.x=TRUE, all.y = TRUE)

 l2 <- ncol(df)
 crf_label[(l1+1):l2] <- crf[cnt]
}
res <- list(df, crf_label, study_id)
return(res)
}

#' Edit some terms in crf_labels and header of the clinical data
#'
#' @param df 			clinical_df
#' @param crf_label 	ecrfs label
#' @inheritParams 		read_db
#' @return 		edited df, mapping table (including the tranSMART path and labels)
#' @author 		Sepideh
#' @export

edit_labels <- function(df, crf_label) {

# add something to OMIC to treat it as the categorical variable
df$OMICID  <- paste0("N",df$OMICID)

# this is needed for removing duplicated parameters

# colnames(df)[which(names(df) == "VISIT.sampling")] <- "VISIT"
colnames(df)[which(names(df) == "CENTER.sampling")] <- "CENTER"
 colnames(df)[which(names(df) == "USUBJID.sampling")] <- "USUBJID"
colnames(df)[which(names(df) == "ONSETDT")] <- "ONSET.DT"
colnames(df)[which(names(df) == "LBDT")] <- "LB.DT"

df[is.na(df)] <- ""

crf_label <- gsub("\\.","", crf_label)
##col_path<- paste0(crf_label, "+")
col_path<- crf_label

##names(df) <- gsub("_","\\.", names(df))
colnames(df)[1] <- "SUBJ_ID"

colnames(df)[2] <- "VISIT_NAME"

col_name <- names(df)
#col_path<- paste0(col_path, col_name)

map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
names(map_data) <- c("category_cd", "data_label")

res <- list(df, map_data)
return(res)
}


#' Remove redundant variables from the map_data
#'
#' @param map_data		mapping table (from edit_labels)
#' @inheritParams 		edit_labels
#' @return 				clean mapping table
#' @author 			Sepideh
#' @export

remove_extra_var <- function(map_data) {

extra_var <- c("SUBJ_ID", "VISIT_NAME", ".cmrbdt", ".concmed", ".consentc", ".demog", ".diagnos", ".lab", ".sampling", ".sympt",
			   "DCON", "MCON", "YCON", "CONDAT", "AGEU","ONSET_DT", "ONSETDY", "ONSETMO", "ONSETYR", "ONSET.DT", "LB.DT",
			   "DONE1YN", "DDATE1","MDATE1","YDATE1","LB_DT", "BRTHDTC", "LACTDTC", "DSAMPL", "CMDOSEU", "CENT_ID", "COUN_ID",
			   "MSAMPL", "YSAMPL", "DSAMPLDAT", "SAMPLDAT.", "BMI_DU", "HEIGU", "WEIGHTU",
			   ".heart", ".muske",".gastro", ".kidney", ".lung", ".nervsys", ".skinmuc", ".vascular")

for (cnt in 1: length(extra_var)){
sel <- grep(extra_var[cnt], map_data[,2])
if(length(sel)>0){
map_data[-sel,]
map_data <- map_data[-sel,]
}
}

tmp <- data.frame(category_cd= c(" ", " ", " "), data_label= c("STUDY_ID","SUBJ_ID", "VISIT_NAME"))

map_data <- rbind(tmp, map_data)

return(map_data)
}


#' Organize the tranSMART tree structure
#'
#' @param map_data		mapping table
#' @inheritParams 		remove_extra_var
#' @return 				edited mapping table
#' @author 		Sepideh
#' @export

organize_tree <- function(map_data) {

old_var <- c("cmrbdt", "concmed", "consentc", "demog", "diagnos", "lab", "sampling", "sympt","heart", "muske","gastro", "kidney", "lung", "nervsys", "skinmuc", "vascular")
new_var <- c( "Comorbidity", "Medication", "Consent", "Demography", "Diagnosis", "Lab", "Sampling", "Symptom", "Heart", "Muscle_and_Skeletal", "Gastro", "Kidney", "Lung", "Nerve_System", "Skin", "Vascular" )
for (cnt in 1: length(old_var)){
map_data[,1] <- gsub(old_var[cnt],new_var[cnt], map_data[,1])
}

return(map_data)
}

#' Upload clinical data from the db into transmart
#'
#'
#' @param path				the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/.."), study type
#' @param study_type		"cross_sectional" or "inception"
#'
#' @return 					df of all clinical data sorted by SUBJ_ID and VISIT, study_id, mapping table
#'
#' @author Sepideh
#' @export

upload_ecrfs_data <- function (DB, study_type)
{

res <- read_db(DB, study_type)
df<-res[[1]]
crf_labels <-res[[2]]
study_id <- res[[3]]

res <- edit_labels(df, crf_labels)
df<-res[[1]]
map_data <-res[[2]]

map_data <- remove_extra_var(map_data)
map_data <- organize_tree(map_data)

map_data$category_cd[-c(1,2,3)]<- paste0("Clinical+", map_data$category_cd[-c(1,2,3)])



res <- list(df, map_data, study_id)
return(res)
}


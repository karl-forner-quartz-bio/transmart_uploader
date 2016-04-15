#' upload clinical data from the db_clinical into transmart
#'
#'   PRECISESADS db_clincial consists of ALL ecrfs of patients with unique patient ids,
#'
#' @param path				the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/db_clinical.db"), study type			
#' @return 					crf_label, df of all clinical data sorted by SUBJ_ID, study_id
#'
#' @author Sepideh
#' @export

read_db <- function(DB, study_type, ...) {
  
 db = dbConnect(RSQLite::SQLite(), DB)
 dblist <- dbListTables(db)
 dblist_sub <- subset(dblist, grepl(study_type, dblist))
 
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
 df <- merge(df, tmp, by = "SUBJ_ID", suffixes = c(crf[cnt-1], crf[cnt]), all.x= TRUE, all.y = TRUE)
 l2 <- ncol(df)
 crf_label[(l1+1):l2] <- crf[cnt]
}
res <- list(df, crf_label, study_id)
return(res)
}

#' edit some terms in crf_labels and header of the df of clinical data
#'
#' @param  clinical_df, crf_label		
#' @return  edited df, map_data (including the tranSMART path and labels)
#' @author Sepideh
#' @export
edit_labels <- function(df, crf_label) {

# add something to OMIC to treat it as the categorical variable
df$OMICID  <- paste0("N",df$OMICID)

# this is needed for removing duplicated parameters
 colnames(df)[which(names(df) == "VISIT.sampling")] <- "VISIT"
 colnames(df)[which(names(df) == "ONSETDT")] <- "ONSET.DT"
 colnames(df)[which(names(df) == "LBDT")] <- "LB.DT"
 
df[is.na(df)] <- ""

crf_label <- gsub("\\.","", crf_label)
col_path<- paste0(crf_label, "+")

names(df) <- gsub("_","\\.", names(df))
colnames(df)[1] <- "SUBJ_ID"
col_name <- names(df)
col_path<- paste0(col_path, col_name)

map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
names(map_data) <- c("category_cd", "data_label")

res <- list(df, map_data)
return(res)   
}


#' Remove redundant variables from the map_data
#'
#' @param map_data		
#' @return map_data		
#' @author Sepideh
#' @export

remove_extra_var <- function(map_data) {

extra_var <- c("SUBJ_ID", "STUDY.ID","RECORDID", "FLAG", "PAGENO", "PAGEREP", "STATUS", "PBLANK", "COUN.ID", "CENT.ID", "ENTRY.ID", "ENTRYDTC", "CENTER.", 
"USUBJID.", "VISIT.", "DCON", "MCON", "YCON", "CONDAT", "AGEU","ONSET.DT", "ONSETDY", "ONSETMO", "ONSETYR", "DONE1YN","DDATE1","MDATE1","YDATE1","LB.DT", 
"BRTHDTC", "LACTDTC" )

for (cnt in 1: length(extra_var)){
sel <- grep(extra_var[cnt], map_data[,2])
map_data[-sel,]
map_data <- map_data[-sel,]
}

tmp <- data.frame(category_cd= c(" ", " "), data_label= c("STUDY_ID","SUBJ_ID"))
map_data <- rbind(tmp, map_data)

return(map_data)
}


#' Organize the tranSMART tree structure
#'
#' @param map_data		
#' @return map_data		
#' @author Sepideh
#' @export

organize_tree <- function(map_data) {

old_var <- c("cmrbdt", "concmed", "consentc", "demog", "diagnos", "lab", "sampling", "sympt" )
new_var <- c( "Comorbidity", "Medication", "Consent", "Demography", "Diagnosis", "Lab", "Sampling", "Symptom")
for (cnt in 1: length(old_var)){
map_data[,1] <- gsub(old_var[cnt],new_var[cnt], map_data[,1])
}

return(map_data)
}





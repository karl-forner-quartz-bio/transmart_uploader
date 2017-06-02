#' Read metabolomics data from the db to upload in transmart
#'
#' @param DB                            the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/..")
#' @param study_type                    study type can be inception or ceoss_sectional
#'
#' @return                              data frame of metabolomics values and mapping table
#'
#' @author Sikander
#' @export

create_mapfile <- function(DB, study_type, studyid, ...) {
db = dbConnect(RSQLite::SQLite(), DB)
dblist <- dbListTables(db)
dblist_sub <- subset(dblist, grepl(study_type, dblist))
df <- dbReadTable(db,dblist_sub[1])

df<- subset(df, select = c(OMICID, SUBJ_ID))
sub_ids <- c(df$SUBJ_ID)
omic_ids<- c(df$OMICID)

n = length(sub_ids)

study_id <- rep(studyid, n)               ## TODO change function such that AD1=cs, AD2=inception
site_id <- rep(" ", n)
platform <- rep("METABOLOMICS_CUSTOM_PLATFORM", n)      ## TODO make it take platform as an input to the function
tissue_ids <- rep("Plasma", n)
attr1 <- rep(" ", n)
attr2 <- rep(" ", n)
category_cd <- rep("Metabolomics data", n)
source_cd <- rep(" ", n)

newdata <- data.frame(study_id, site_id, sub_ids, omic_ids, platform, tissue_ids, attr1, attr2, category_cd, source_cd, stringsAsFactors = FALSE)
newdata_colnames = c("STUDY_ID", "SITE_ID", "SUBJECT_ID", "SAMPLE_CD", "PLATFORM", "TISSUETYPE", "ATTRIBUTE_1", "ATTRIBUTE_2", "CATEGORY_CD", "SOURCE_CD")
names(newdata) <- newdata_colnames

#write.table(newdata, file="STUD_Subject_Sample_Mapping_File.txt", sep = "\t", row.names = FALSE, quote=FALSE)

return(newdata)

}



#' Read metabolomics data from the db to upload in transmart
#'
#' @param DB                            the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/..")
#' @param study_type                    study type can be inception or ceoss_sectional
#'
#' @return                              data frame of metabolomics values and mapping table
#'
#' @author Sikander
#' @export

create_metabolomics_pathfile <- function(DB, study_type, ...) {
db = dbConnect(RSQLite::SQLite(), DB)
dblist <- dbListTables(db)
dblist_sub <- subset(dblist, grepl(study_type, dblist))
df <- dbReadTable(db,dblist_sub[1])

df<- subset(df, select = -OMICID)
df<- subset(df, select = -VISIT_NAME)

sub_ids <-  c(df$SUBJ_ID)
df<- subset(df, select = -SUBJ_ID)

inx1 <- grep("SUBJ_ID", names(df))                     # gives index of SUBJ_ID col
inx2 <- grep("VISIT_NAME", names(df))                  # gives index of "VISIT_NAME" col
col_name <- names(df)[-c(inx1,inx2)]

#df_trans = t(df[-1])
df_trans = as.data.frame(t(df))
colnames(df_trans) = sub_ids

#df_trans_with_biochemical <- cbind(BIOCHEMICAL = rownames(df_trans), df_trans)
#biochemical <- df_trans_with_biochemical$BIOCHEMICAL

n = nrow(df_trans)
biochemical <- c(rownames(df_trans))

#super_pathway <- vector(mode="character", length=n)
super_pathway <- rep("lipid metabolism", n)

#sub_pathway <- vector(mode="character", length=n)
sub_pathway <- rep("sphingolipid metabolism", n)

#hmdb <- vector(mode="character", length=n)
hmdb <- rep(" ", n)

newdata <- data.frame(biochemical, super_pathway, sub_pathway, hmdb, stringsAsFactors = FALSE)
newdata_colnames = c("BIOCHEMICAL", "SUPER_PATHWAY", "SUB_PATHWAY", "HMDB_ID")
names(newdata) <- newdata_colnames

#filepath="/home/shayat/20161117_transmart/metabolomics_skeleton/code/CUSTOM_PLATFORM_1.txt"
#write.table(newdata, file=filepath, sep = "\t", row.names = FALSE, quote = FALSE)

return(newdata)                 ##TODO check it later.

}


#' Read metabolomics data from the db to upload in transmart
#'
#' @param DB                            the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/..")
#' @param study_type                    study type can be inception or ceoss_sectional
#'
#' @return                              data frame of metabolomics values and mapping table
#'
#' @author Sikander
#' @export

read_metabolomics_db <- function(DB, study_type, ...) {
db = dbConnect(RSQLite::SQLite(), DB)
dblist <- dbListTables(db)
dblist_sub <- subset(dblist, grepl(study_type, dblist))
df <- dbReadTable(db,dblist_sub[1])

omic_ids <-  c(df$OMICID)
df<- subset(df, select = -OMICID)
df<- subset(df, select = -VISIT_NAME)

sub_ids <-  c(df$SUBJ_ID)
df<- subset(df, select = -SUBJ_ID)

inx1 <- grep("SUBJ_ID", names(df))                     # gives index of SUBJ_ID col
inx2 <- grep("VISIT_NAME", names(df))                  # gives index of "VISIT_NAME" col
col_name <- names(df)[-c(inx1,inx2)]

#df_trans = t(df[-1])
df_trans = as.data.frame(t(df))
#colnames(df_trans) = sub_ids
colnames(df_trans) = omic_ids

#df_trans_with_biochemical <- cbind(BIOCHEMICAL = rownames(df_trans), df_trans)
#biochemical <- df_trans_with_biochemical$BIOCHEMICAL

n = nrow(df_trans)
biochemical <- c(rownames(df_trans))

#super_pathway <- vector(mode="character", length=n)
super_pathway <-  rep("lipid metabolism", n)

#sub_pathway <- vector(mode="character", length=n)
sub_pathway <-  rep("sphingolipid metabolism", n)

#comp_id <- vector(mode="character", length=n)
comp_id <-  rep(" ", n)

#platform <- vector(mode="character", length=n)
platform <-  rep("METABOLOMICS_CUSTOM_PLATFORM", n)

#ri <- vector(mode="character", length=n)
ri <-  rep(" ", n)

#mass <- vector(mode="character", length=n)
mass <-  rep(" ", n)

#lmsd <- vector(mode="character", length=n)
lmsd <-  rep(" ", n)

 #hmdb <- vector(mode="character", length=n)
hmdb <- rep(" ", n)

#pubchem  <- vector(mode="character", length=n)
pubchem <- rep(" ", n)

#chebi <- vector(mode="character", length=n)
chebi <-  rep(" ", n)

newdata <- data.frame(biochemical, super_pathway, sub_pathway, comp_id, platform, ri, mass, lmsd, hmdb, pubchem, chebi, stringsAsFactors = FALSE)
newdata_colnames = c("BIOCHEMICAL", "SUPER_PATHWAY", "SUB_PATHWAY", "COMP_ID", "PLATFORM", "RI", "MASS", "LMSD", "HMDB", "PUBCHEM", "ChEBI")
names(newdata) <- newdata_colnames

final_set <- cbind(newdata, df_trans)

#write.table(final_set, file="STUD_Metabolomics_Data_T.txt", sep = "\t", row.names = FALSE, quote=FALSE)

return(final_set)

}

                

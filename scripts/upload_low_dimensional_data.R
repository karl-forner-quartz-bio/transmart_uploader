
rm(list = ls())

devtools::load_all('TransmartUploader')

# study_type = "cross_sectional"
study_type = "inception"

DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_clinical.db'
res <- upload_ecrfs_data(DB, study_type)
df<-res[[1]]
map_data <-res[[2]]
study_id <- res[[3]]
#\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/
DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_autoantibody_transmart.db'
res <- read_aab_db (DB, study_type)
df_aab <-res[[1]]
map_data_aab <-res[[2]]

if (nrow(df_aab)>0){
df <-merge(df, df_aab, by=c("SUBJ_ID", "VISIT_NAME"), all.x= T)
map_data <-rbind(map_data, map_data_aab)
}
#\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/
DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_autoantibody_transmart.db'
res <- read_ab_db (DB, study_type)
df_ab <-res[[1]]
map_data_ab <-res[[2]]

if (nrow(df_ab)>0){
df <-merge(df, df_ab, by=c("SUBJ_ID", "VISIT_NAME"), all.x= T)
map_data <-rbind(map_data, map_data_ab)
}
#\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/
DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_luminex.db'
res <- read_luminex_db (DB, study_type)
df_lum <-res[[1]]
map_data_lum <-res[[2]]

if (nrow(df_lum)>0){
df <-merge(df, df_lum, by=c("SUBJ_ID", "VISIT_NAME"), all.x= T)
map_data <-rbind(map_data, map_data_lum)
}

#\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/
DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_hla.db'
res <- read_hla_db (DB, study_type)
df_hla <-res[[1]]
map_data_hla <-res[[2]]

if (nrow(df_hla)>0){
df <-merge(df, df_hla, by=c("SUBJ_ID", "VISIT_NAME"), all.x= T)
map_data <-rbind(map_data, map_data_hla)
}

#\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/
DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_flowcytometry.db'
res <- read_fc_db (DB, study_type)
df_fc <-res[[1]]
map_data_fc <-res[[2]]

##VISIT_NAME <-"M000"
##df_fc <- cbind(df_fc, VISIT_NAME)

if (nrow(df_fc)>0){
df <-merge(df, df_fc, by=c("SUBJ_ID", "VISIT_NAME"), all.x= T)
map_data <-rbind(map_data, map_data_fc)
}

#\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/\@/

df[is.na(df)] <- ""

if (study_type =="cross_sectional"){
map_data <- map_data [-which(map_data[,2]=="VISIT_NAME"),]
}



upload_clinical_data(df, study_id = study_id, transmart_path = 'Cross Sectional/Low Dimensional Data', mapping = map_data)

upload_clinical_data(df, study_id = study_id, transmart_path = 'Inception/Low Dimensional Data', mapping = map_data)








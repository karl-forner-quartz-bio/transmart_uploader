#' Read flow cytometry data from the db to upload in transmart
#'
#' @param DB				the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/..")
#' @param study_type		study type can be inception or ceoss_sectional
#'
#' @return 					data frame of fc values and mapping table
#'
#' @author Sepideh
#' @export

read_fc_db <- function(DB, study_type, ...) {
 
 db = dbConnect(RSQLite::SQLite(), DB)
 dblist <- dbListTables(db)
 dblist_sub <- subset(dblist, grepl(paste0(study_type,"_absolute_pop_wide"), dblist))
 
 df <- dbReadTable(db,dblist_sub[1])
 df[is.na(df)] <- ""

df<- subset(df, select = -OMICID)
inx1 <- grep("SUBJ_ID", names(df))
inx2 <- grep("VISIT_NAME", names(df))
# names(df)[-inx] <- gsub("_","\\.", names(df)[-inx])

col_name <- names(df)[-c(inx1,inx2)]

tmp <- unlist(strsplit(col_name, "_"))
col_path <- tmp[grep("^P[0-9]", tmp)]
col_path<- paste0("Flow_cytometry+", col_path)
# col_path<- "Flow_cytometry"

names(df) <- gsub("^P[0-9]_","", names(df))
col_name <- gsub("^P[0-9]_","", col_name)

map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
names(map_data) <- c("category_cd", "data_label")

res <- list(df, map_data)
return(res)

}



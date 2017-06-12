#' Read HLA data from the db to upload in transmart
#'
#' @param	DB			the db file path ("/PRECISESADS_WP8/P603.01_proj/data/db/..")
#' @param	study_type	study type can be inception or ceoss_sectional		
#' @return 				data frame of hla alleles and indels values and the mapping table
#'
#' @author Sepideh
#' @export

read_hla_db <- function(DB, study_type, ...) {
 
 ## DB ='/PRECISESADS_WP8/P603.01_proj/data/db/db_hla.db'
 db = dbConnect(RSQLite::SQLite(), DB)
 dblist <- dbListTables(db)
 dblist_sub <- subset(dblist, grepl(paste0(study_type,"_hla_alleles"), dblist))
 
 df1 <- dbReadTable(db,dblist_sub[1])
 col_name <- names(df1)[-c(1,2,3)]
 col_path<- "HLA+Alleles"

 map_data1 <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
 names(map_data1) <- c("category_cd", "data_label")


 dblist_sub <- subset(dblist, grepl(paste0(study_type,"_hla_indels"), dblist))
 df2 <- dbReadTable(db,dblist_sub[1])

 col_name <- names(df2)[-c(1,2,3)]
 col_path<- "HLA+Indels"

 map_data2 <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
 names(map_data2) <- c("category_cd", "data_label")

 df <- merge(df1,df2, by=c( "OMICID", "SUBJ_ID", "VISIT_NAME"), all.x = T, all.y= T)
 df[is.na(df)] <- ""
 map_data <- rbind(map_data1, map_data2)

 df<- subset(df, select = -OMICID)
 
 res <- list(df, map_data)
 return(res)
}


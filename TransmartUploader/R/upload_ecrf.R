
#' upload ecrfs from the db
#'
#'
#' @param path		the db path, transmart_path, study_type, transmart_path = 'Cross_Sectional/Clinical_Data'
#' @inheritParams   upload_clinical_data
#' @return 		summary statistics of uploaded data
#'
#' @author Sepideh
#' @export
#' @family CRFs

upload_ecrf <- function(DB, transmart_path, study_type, ...) {

res <- read_db(DB, study_type)
df<-res[[1]]
crf_labels <-res[[2]]
study_id <- res[[3]]

res <- edit_labels(df, crf_labels)
df<-res[[1]]
map_data <-res[[2]]

map_data <- remove_extra_var(map_data)
map_data <- organize_tree(map_data)

 
upload_clinical_data(df, study_id = study_id, transmart_path = transmart_path, mapping = map_data)
}



#' Generat data.fram to make the mapping file to upload the data to tranSMART 
#' This is the default map and shoulddited based on our tranSMARt tree structure
#'


#' @return map_data   two cols data.frame indicating two fixed col name: "category_cd", "data_label"
#'
#' @author Sepideh


default_mapping <- function()
{

ColPath <- " / /Tissue/Subject+Demographic/Subject+Phenotype"
ColName <- "STUDY_ID/SUBJ_ID/Tissue_Type/Sex/Phenotype" 

ColPath <-(unlist(strsplit(ColPath, "/")))
ColName <- (unlist(strsplit(ColName, "/")))

map_data <- data.frame(ColPath, ColName, stringsAsFactors=FALSE)
names(map_data) <- c("category_cd", "data_label")


return(map_data)
}

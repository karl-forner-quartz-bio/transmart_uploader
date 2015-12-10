
#' Generate the mapping of variables to the TranSMART tree fo FC data
#'
#' this mapping is mandatory to upload the data into tranSMART
#' @return map_data   data.frame with columns "category_cd" and "data_label"
#'
#' @author Sepideh
#' @export
FCs_mapping <- function(Fcs) {

path0 = "FlowCytometry+"
col_path <-  gsub("__","+", colnames(FCs))
col_name <- colnames(FCs)

col_path<- paste0(path0, col_path)
col_path <- c(" ", col_path)
col_name <- c("STUDY_ID",col_name )
map_data <- data.frame(col_path, col_name, stringsAsFactors = FALSE)
map_data[2,1] <- ""
names(map_data) <- c("category_cd", "data_label")

  map_data
}


    
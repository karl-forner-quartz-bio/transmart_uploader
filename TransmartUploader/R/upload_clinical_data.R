
#' run the tMDataLoader tool (run_tm_etl) to upload clinical data (no high dimensional data)
#'
#' @param dfs				the data to upload
#' @param study_id			study_id: STUDY ID need to be specified by user
#' @param mapping			the corresponding mapping
#' @param tissue_typ			tissue_type of data
#' @param transmart_path		the path in etl tree, as a standard path string
#' @param ...				additional arguments to \code{execute_etl_cmd}
#'
#' @return the upload summary statistics as a data frame, or NULL
#'
#' @author Sepideh
#' @export



upload_clinical_data <- function(
  dfs,
  study_id, 
  mapping = default_mapping(),
  tissue_type = " ",
  transmart_path = paste("Tests", study_id, sep = "_"),
  ...
){
  
# STUDY_ID needs to specify by the user. There is no default value for this input.
    if (missing(study_id)) stop("STUDY ID need to be specified.")
  
  #add "...ToUpload" to the path
  transmart_path = paste(transmart_path,"/ClinicalDataToUpload", sep = "")
  
  #add tissu-type column to the data frames if it is not in the original input
  dfs <- lapply(dfs, tissue_type = tissue_type, check_tissue_type)
    
  #Add STUDY_ID as a column to the data frames
  dfs <- lapply(STUDY_ID = study_id, dfs, cbind, stringsAsFactors = FALSE)
    
  # call Mapping function ase defualt
  df_map <- mapping
  
  res <- run_tm_etl(dfs, df_map, etl_path = transmart_path)


  res
}


# Check if tissue type exist
check_tissue_type <- function (df, tissue_type = tissue_type)
{
  
if (!("Tissue_Type" %in% colnames(df)))
df <-cbind(df, Tissue_Type = tissue_type, stringsAsFactors  =FALSE)

 df
}







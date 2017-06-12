
#' upload tissue_expression_data
#'
#'
#' @param 			expresion data, platform text, clinical data
#' @inheritParams   upload_gene_expression_data

#'
#' @author Sepideh
#' @export

tissue_expression_data <- function (platform, df, clinical_data, platform_name, study_id, transmart_path){


#df first col must be REF_ID
colnames(df)[1]<- "ID_REF"

map_df <- data.frame(cbind(STUDY_ID=study_id, SITE_ID = " ", SUBJ_ID = clinical_data$SUBJ_ID, SAMPLE_CD= colnames(df[-1]), 
                PLATFORM= platform_name, TISSUETYPE="Kidney", ATTR1=" ",ATTR2=" ", category_cd= "Biomarker_Data+Gene_Expression+TISSUETYPE"), stringsAsFactors = FALSE)

upload_gene_expression_data(platform = platform, df, study_id, map_df = map_df, transmart_path = transmart_path) 

}
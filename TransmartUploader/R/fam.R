
#' curate data in fam format to upload to tranSMART
#'
#' @param fam_data		the data.frame read from a "*.fam" file


#' @return sample_data		the data.frame with 3 cols, fixed col names: "SUBJ_ID", "Sex", "Phenotype"
#'
#' @author Sepideh


read_fam <- function(fam_data)
{
 #fam_data = dfs[[1]]
  
if (!(ncol(fam_data) == 6)) stop('Number of columns in Fam file is not correct')
if ((length(unique(fam_data[,1])) == 1) && (length(unique(fam_data[,2])) == 1)) stop('There is no SUBJECT ID')

inx <- 1
if (length(unique(fam_data[,inx])) == 1) inx <- 2
SUBJ_ID <- as.character(fam_data[,inx])

inx <- grep(1,fam_data[,5])
if (length(inx) > 0 ) fam_data[inx,5] <- 'Male'

inx <- grep(2,fam_data[,5])
if (length(inx) > 0 ) fam_data[inx,5] <- 'Female'

inx <- grep(1,fam_data[,6])
if (length(inx) > 0 ) fam_data[inx,6] <- 'Control'

inx <- grep(2,fam_data[,6])
if (length(inx) > 0 ) fam_data[inx,6] <- 'Case'


sample_data <- cbind(SUBJ_ID, fam_data[,5:6], stringsAsFactors=FALSE)
sample_data[sample_data==0] <- NA

names(sample_data) <- c("SUBJ_ID", "Sex", "Phenotype")

return(sample_data)
}
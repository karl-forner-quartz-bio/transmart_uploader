#' upload a FAM file into transmart
#'
#' a FAM file is a PLINK individual information file,
#' cf \url{http://pngu.mgh.harvard.edu/~purcell/plink2/formats.html#fam}
#'
#' @param path		the FAM file path
#' @inheritParams upload_clinical_data
#' @return sample_data		the data.frame with 3 cols, fixed col names: "SUBJ_ID", "Sex", "Phenotype"
#'
#' @author karl
#' @export
#' @family fam
upload_fam <- function(path, transmart_path, study_id, ...) {
  raw <- read_fam(path)
  fam <- format_fam(raw)
  upload_clinical_data(fam, study_id, transmart_path = transmart_path, ...)
}

#' fetch the fam test file
#'
#' @return a file path
#'
#' @author karl
#' @export
fetch_ref_fam <- function() { find_extdata_file('sample_data/fam/ref.fam') }




#' read a FAM file
#'
#' @param path		the FAM file path
#' @return the file content as a data frame
#' @author Karl
#' @family fam
#' @export
read_fam <- function(path) {
  read.table(path, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
}


#' curate fam data to upload to tranSMART
#'
#'
#' @param fam_data				the data.frame read from a "*.fam" file
#' @return sample_data		the data.frame with 3 cols, fixed col names: "SUBJ_ID", "Sex", "Phenotype"
#'
#' @author Sepideh
#' @export
#' @family fam
format_fam <- function(fam_data) {
   #fam_data = dfs[[1]]

  if (!(ncol(fam_data) == 6)) stop('Number of columns in Fam file is not correct')
  if ((length(unique(fam_data[, 1])) == 1) && (length(unique(fam_data[, 2])) == 1))
    stop('There is no SUBJECT ID')

  # choose the ID column
  ids <- fam_data[[1]]
  if (anyDuplicated(ids) != 0) ids <- fam_data[[2]]
  if (anyDuplicated(ids) != 0) stop("Error: duplicated ids in FAM data")

  ids <- as.character(ids)

  sex <- recode(fam_data[[5]], "1='Male'; 2='Female';else=NA",
    as.numeric.result = FALSE)

  pheno <- recode(fam_data[[6]], "1='Control'; 2='Case';else=NA",
    as.numeric.result = FALSE)

  res <- data.frame(SUBJ_ID = ids, Sex = sex, Phenotype = pheno,
    stringsAsFactors = FALSE)

  res
}
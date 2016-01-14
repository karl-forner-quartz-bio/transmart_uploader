#' upload 3 crfs diagnis, sampling and demog into transmart
#'
#'
#' @param path		the data folder path including 3 ctfs in .txt format
#' @inheritParams upload_clinical_data
#' @return sample_data		summary statistics of uploaded data
#'
#' @author Sepideh
#' @export
#' @family CRFs
upload_crfs <- function(path, transmart_path, study_type, ...) {

  raw <- read_diagnos(paste0(path, "/diagnos.sas7bdat.txt"))
  diagnos  <- format_diagnos(raw)
  study_id <- unique(raw$STUD_ID)
  map_di <- diagnos_mapping(study_type)

  raw <- read_sampling(paste0(path, "/sampling.sas7bdat.txt"))
  sampling  <- format_sampling(raw)
  map_samp <- sampling_mapping(study_type)

  raw <- read_demog(paste0(path, "/demog.sas7bdat.txt"))
  demog  <- format_demog(raw)
  map_dem <- demog_mapping(study_type)

  raw <- read_consentc(paste0(path, "/consentc.sas7bdat.txt"))
  consent  <- format_consentc(raw)
  map_con <- consentc_mapping(study_type)

  df <- merge(diagnos, sampling, by = c("SUBJ_ID", "PatientID"))
  df <- merge(df, demog, by = c("SUBJ_ID", "PatientID"))
  df <- merge(df, consent, by = c("SUBJ_ID", "PatientID"))

  maps <- rbind( map_di, map_samp, map_dem, map_con)
  maps <- maps[!duplicated(maps), ]

  upload_clinical_data(df, study_id, transmart_path = transmart_path, mapping = maps)
}

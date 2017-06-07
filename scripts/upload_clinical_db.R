# upload data from PreciseSads clinical DB
###############################################################################

library(devtools)
library(RSQLite)

document('TransmartUploader')
load_all('TransmartUploader')

study_type <- "inception"
DB <- 'db/db_clinical.db'

tbls <- read_tables_for_cohort(DB, 'inception')
# remove prefix
names(tbls) <- sub('^.+_', '', names(tbls))
# remove flag and mapping tables
tbls$flag <- tbls$mapping <- NULL

names(tbls) <- prettify_clinical_tables(names(tbls))
repeated_vars <- get_repeated_vars(tbls)
# keep STUDY_ID and SUBJ_ID
repeated_vars <- setdiff(repeated_vars, c('STUDY_ID', 'SUBJ_ID'))

# tbls with unique vars
tbls_unique <- lapply(tbls, remove_vars_from_df, repeated_vars)



# read all tables rfron a given cohort (CS/IC)
# output a named list of data frames
read_tables_for_cohort <- function(db_path, cohort = c('inception', 'cross_sectional')) {
  cohort <- match.arg(cohort)

  db <- dbConnect(RSQLite::SQLite(), db_path)
  tbls <- dbListTables(db)
  tbls <- grep(paste0('^', cohort, '_'), tbls, value = TRUE)

  # N.B: sapply to get names
  sapply(tbls, dbReadTable, conn = db, simplify = FALSE)
}


prettify_clinical_tables <- function(tbl_names) {
  hash <- list(cmrbdt = "Comorbidity", concmed = "Medication",
    consentc = "Consent", demog = "Demography", diagnos = "Diagnosis",
    lab = "Lab", sampling = "Sampling", sympt = "Symptom", heart = "Heart",
    muske = "Muscle_and_Skeletal", gastro = "Gastro", kidney = "Kidney",
    lung = "Lung", nervsys = "Nerve_System", skinmuc = "Skin",
    vascular = "Vascular")

  lst <- as.character(hash[tbl_names])
  stopifnot(length(lst) ==length(tbl_names))

  lst
}

# get the vars that are duplicated among tables
get_repeated_vars <- function(tbls) {
  all_vars <- unlist(lapply(tbls, colnames), use.names = FALSE)
  unique(all_vars[duplicated(all_vars)])
}

remove_vars_from_df <- function(df, vars) {
  df[setdiff(names(df), vars)]
}

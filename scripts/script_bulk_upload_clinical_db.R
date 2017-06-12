# upload data from PreciseSads clinical DB
###############################################################################

library(devtools)
library(RSQLite)

load_all('TransmartUploader')

################## PARAMETERS ##############################################

DB <- 'db/db_clinical.db'
HOST <- 'pyro'
COHORT <- 'inception'
STUDY_ID <- 'AD2'
TABLES_TO_REMOVE <- c('flag', 'mapping')
VARS_TO_REMOVE <- c(
	"DCON", "MCON", "YCON", "CONDAT", "AGEU","ONSET_DT", "ONSETDY", "ONSETMO", "ONSETYR",
	"DONE1YN", "DDATE1","MDATE1","YDATE1","LB_DT", "BRTHDTC", "LACTDTC", "DSAMPL", "CMDOSEU", "CENT_ID", "COUN_ID",
	"MSAMPL", "YSAMPL", "DSAMPLDAT", "SAMPLDAT.", "BMI_DU", "HEIGU", "WEIGHTU")

VARS_TO_KEEP <- list(sampling = c('CENTER', 'USUBJID'))
PRETTY_TABLE_NAMES <- list(cmrbdt = "Comorbidity", concmed = "Medication",
  consentc = "Consent", demog = "Demography", diagnos = "Diagnosis",
  lab = "Lab", sampling = "Sampling", sympt = "Symptom", heart = "Heart",
  muske = "Muscle_and_Skeletal", gastro = "Gastro", kidney = "Kidney",
  lung = "Lung", nervsys = "Nerve_System", skinmuc = "Skin",
  vascular = "Vascular")

############# FUNCTIONS ####################################################
# read all tables rfron a given cohort (CS/IC)
# output a named list of data frames
read_tables_for_cohort <- function(db_path, cohort = c('inception', 'cross_sectional')) {
  cohort <- match.arg(cohort)
  db <- dbConnect(RSQLite::SQLite(), db_path)
  tbls <- grep(paste0('^', cohort, '_'), dbListTables(db), value = TRUE)

  sapply(tbls, dbReadTable, conn = db, simplify = FALSE)
}

#################### SCRIPT ################################################


### STEPS:
# - extract named list of tables
# - remove some tables
# - rename all columns xxxDT to xxx.DT
# - rename VISIT to VISIT_NAME
# - remove some extra vars
# - bulk upload


### extract named list of tables
tbls <- read_tables_for_cohort(DB, COHORT)
names(tbls) <- sub('^.+_', '', names(tbls)) # remove prefix

### remove some tables
tbls <- tbls[setdiff(names(tbls), TABLES_TO_REMOVE)]

### rename all columns xxxDT to xxx.DT
fix_date_column_names <- function(df) setNames(df, sub('DT$', '.DT', names(df)))
tbls <- sapply(tbls, fix_date_column_names, simplify = FALSE)

### rename VISIT to VISIT_NAME
fix_visit_name <- function(df) {
  ndf <- names(df)
  if ('VISIT' %in% ndf) {
    ndf[ndf == 'VISIT'] <- 'VISIT_NAME'
    names(df) <- ndf
  }

  df
}
tbls <- sapply(tbls, fix_visit_name, simplify = FALSE)

pretty_names <- as.character(PRETTY_TABLE_NAMES[names(tbls)])


tt <- system.time(
  res <- bulk_upload_clinical_data(tbls, STUDY_ID,
    etl_path = 'Inception/Low Dimensional Data',
    categories =  paste0('Clinical+', pretty_names),
    keep = VARS_TO_KEEP, host = HOST)
)
print(tt)

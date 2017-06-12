# upload data from PreciseSads clinical DB
###############################################################################

library(devtools)
library(RSQLite)

document('TransmartUploader')
load_all('TransmartUploader')

DB <- 'db/db_clinical.db'
HOST <- 'pyro'
COHORT <- 'inception'
STUDY_ID <- 'AD2'
TABLES_TO_REMOVE <- c('flag', 'mapping')
MANDATORY_VARS <- c('STUDY_ID', 'SUBJ_ID', 'VISIT_NAME')
VARS_TO_REMOVE <- c(
	"DCON", "MCON", "YCON", "CONDAT", "AGEU","ONSET_DT", "ONSETDY", "ONSETMO", "ONSETYR",
	"DONE1YN", "DDATE1","MDATE1","YDATE1","LB_DT", "BRTHDTC", "LACTDTC", "DSAMPL", "CMDOSEU", "CENT_ID", "COUN_ID",
	"MSAMPL", "YSAMPL", "DSAMPLDAT", "SAMPLDAT.", "BMI_DU", "HEIGU", "WEIGHTU")

SAMPLING_VARS_TO_KEEP <- c('CENTER', 'USUBJID')
PRETTY_TABLE_NAMES <- list(cmrbdt = "Comorbidity", concmed = "Medication",
  consentc = "Consent", demog = "Demography", diagnos = "Diagnosis",
  lab = "Lab", sampling = "Sampling", sympt = "Symptom", heart = "Heart",
  muske = "Muscle_and_Skeletal", gastro = "Gastro", kidney = "Kidney",
  lung = "Lung", nervsys = "Nerve_System", skinmuc = "Skin",
  vascular = "Vascular")

### STEPS:
# - extract named list of tables
# - remove some tables
# - rename all columns xxxDT to xxx.DT
# - rename VISIT to VISIT_NAME
# - fetch list of duplicated columns across tables
# - backup columns to be kept (i.e sampling$CENTER to sampling$CENTER.KEEP)
# - remove all duplicated columns and predefined useless columns minus the
#   mandatory columns
# - restore columns to keep
# - prettify table names

### extract named list of tables
tbls <- read_tables_for_cohort(DB, COHORT)
names(tbls) <- sub('^.+_', '', names(tbls)) # remove prefix
s
### remove some tables
tbls <- tbls[setdiff(names(tbls), TABLES_TO_REMOVE)]

### rename all columns xxxDT to xxx.DT
tbls <- sapply(tbls, fix_date_column_names, simplify = FALSE)

### rename VISIT to VISIT_NAME
tbls <- sapply(tbls, fix_visit_name, simplify = FALSE)


load_all('TransmartUploader')
categories <- paste0('Clinical+', as.character(PRETTY_TABLE_NAMES[names(tbls)]))
system.time(res <- bulk_upload_clinical_data(tbls, 'AD2', 'Inception/Low Dimensional Data',
  categories = categories,
  keep = list(sampling = SAMPLING_VARS_TO_KEEP), host = DBHOST))
#    user  system elapsed
#  21.784   1.318 135.553




### fetch list of duplicated columns across tables
duplicated_vars <- get_duplicated_vars(tbls)

### backup sampling
sampling <- tbls$sampling

### remove all duplicated columns and predefined useless columns
to_remove <- setdiff(unique(c(duplicated_vars, VARS_TO_REMOVE)), MANDATORY_VARS)
tbls <- sapply(tbls, remove_vars_from_df, to_remove, simplify = FALSE)

### restore columns to keep
tbls$sampling[SAMPLING_VARS_TO_KEEP] <- sampling[SAMPLING_VARS_TO_KEEP]

### prettify table names
names(tbls) <- as.character(PRETTY_TABLE_NAMES[names(tbls)])

names(tbls) <- paste0('Clinical+', names(tbls))

categories <- paste0('Clinical+', as.character(PRETTY_TABLE_NAMES[names(tbls)]))

load_all('TransmartUploader')
res <- bulk_upload_clinical_data(tbls, 'Inception/Low Dimensional Data')

categ <- TransmartUploader:::multi_categorization(tbls, categories)

# trick to keep some columns
vars <- names(tbls$sampling)
ind <- match(SAMPLING_VARS_TO_KEEP, vars)
vars[ind] <- paste0(vars[ind], '.KEEP')
names(tbls$sampling) <- vars

df <- TransmartUploader:::merge_data_dfs(tbls, by = c('SUBJ_ID', 'VISIT_NAME'))


categs <- mapply(simple_categorization, tbls, paste0('Clinical+', names(tbls)),
 SIMPLIFY = FALSE)
categ <- do.call(rbind.data.frame, categs)
row.names(categ) <- NULL


delete_study_by_path('Inception', host = HOST)
for (i in seq_along(tbls)) {
  tbl <- tbls[[i]]
  tbl <- format_input_data(tbl, STUDY_ID)
  tblname <- names(tbls)[i]
  message('uploading ', tblname)
  res <- upload_clinical_data(tbl, 'Inception/Low Dimensional Data',
      paste0('Clinical+', tblname),
       host = HOST, merge = 'APPEND')
}


### debug
med <- tbls[[2]]
med <- format_input_data(med, STUDY_ID)
res2 <- upload_clinical_data(med, 'Inception/Low Dimensional Data',
  paste0('Clinical+', 'Medication'),
  host = HOST, merge = 'APPEND')

#### try to upload the list

mytbls <- tbls[1:2]

.mapit <- function(tbl, fname) { build_mapping_file(tbl, categ, fname) }
mappings <- mapply(.mapit, mytbls, paste0(names(mytbls), '.txt'), SIMPLIFY = FALSE)
mapping <- do.call(rbind, mappings)



res <- upload_clinical_data(mytbls, 'Inception/Low Dimensional Data', mapping = mapping,
  host = HOST, merge = 'UPDATE_VARIABLES')



# read all tables rfron a given cohort (CS/IC)
# output a named list of data frames
read_tables_for_cohort <- function(db_path, cohort = c('inception', 'cross_sectional')) {
  cohort <- match.arg(cohort)
  db <- dbConnect(RSQLite::SQLite(), db_path)
  tbls <- grep(paste0('^', cohort, '_'), dbListTables(db), value = TRUE)

  sapply(tbls, dbReadTable, conn = db, simplify = FALSE)
}

fix_date_column_names <- function(df) {
  setNames(df, sub('DT$', '.DT', names(df)))
}

fix_visit_name <- function(df) {
  ndf <- names(df)
  if ('VISIT' %in% ndf) {
    ndf[ndf == 'VISIT'] <- 'VISIT_NAME'
    names(df) <- ndf
  }

  df
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
get_duplicated_vars <- function(tbls) {
  all_vars <- unlist(lapply(tbls, colnames), use.names = FALSE)
  unique(all_vars[duplicated(all_vars)])
}

remove_vars_from_df <- function(df, vars) {
  df[setdiff(names(df), vars)]
}

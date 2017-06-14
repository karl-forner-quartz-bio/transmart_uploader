# upload flowcyto data from PreciseSads clinical DB
###############################################################################

library(devtools)
library(RSQLite)
load_all('TransmartUploader')

add_categories <- TransmartUploader:::add_categories
################## PARAMETERS ##############################################
DB_ROOT <- 'db'
HOST <- 'pyro'
COHORT <- 'inception'
STUDY_ID <- 'AD2'
ETL_PATH <-  'Inception/Low Dimensional Data'
MERGE <- 'UPDATE_VARIABLES'

### CLINICAL DB
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

############### functions #################################################

db_connect <- function(path) {
  dbConnect(RSQLite::SQLite(), file.path(DB_ROOT, path))
}


#################### ANTIBODY ################################################
db <- db_connect('db_autoantibody_transmart.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'final_ki'))

ab_job <- simple_upload_job(df, STUDY_ID, 'Antibody')


#################### AUTO-ANTIBODY #############################################
db <- db_connect('db_autoantibody_transmart.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'final_ubo'))

aab_job <- simple_upload_job(df, STUDY_ID, 'Autoantibody')

#################### LUMINEX #############################################
db <- db_connect('db_luminex.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'luminex'))

lumi_job <- simple_upload_job(df, STUDY_ID, 'Luminex')


#################### HLA ###################################################
db <- db_connect('db_hla.db')
### ALLELES
df <- dbReadTable(db, paste0(COHORT, '_', 'hla_alleles'))

hla_all_job <- simple_upload_job(df, STUDY_ID, 'HLA+Alleles')

### INDELS
df <- dbReadTable(db, paste0(COHORT, '_', 'hla_indels'))

hla_indels_job <- simple_upload_job(df, STUDY_ID, 'HLA+Indels')

#################### FLOW CYTO #############################################
db <- db_connect('db_flowcytometry.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'absolute_pop_wide'))

df$OMICID <- NULL

df <- format_input_data(df, STUDY_ID)

categ <- simple_categorization(df, ' ')
mapping <- build_mapping_file(df, categ, merge = MERGE)

# columns are prefixed by their panel Pnn. rename them and map them to +Pnn
panels <- sapply(strsplit(mapping$data_label, "_"), getElement, 1)
ind <- grepl('^P\\d', panels)
mapping$category_cd[ind] <- paste0("Flow_cytometry+", panels[ind])
mapping$data_label[ind] <- sub("^P\\d_", "", mapping$data_label[ind])

flowcyto_job <- simple_upload_job(df, STUDY_ID, mapping = mapping)


###################### CLINICAL ##############################################
read_tables_for_cohort <- function(db_path, cohort = c('inception', 'cross_sectional')) {
  cohort <- match.arg(cohort)
  db <- dbConnect(RSQLite::SQLite(), db_path)
  tbls <- grep(paste0('^', cohort, '_'), dbListTables(db), value = TRUE)

  sapply(tbls, dbReadTable, conn = db, simplify = FALSE)
}

### extract named list of tables
tbls <- read_tables_for_cohort(file.path(DB_ROOT, 'db_clinical.db'), COHORT)
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


clinical_job <- multiple_upload_job(tbls, STUDY_ID,
  categories = paste0('Clinical+', pretty_names),
  keep = VARS_TO_KEEP)

#############################################################################

jobs <- list(ab_job, aab_job, lumi_job, hla_all_job, hla_indels_job,
  flowcyto_job, clinical_job)

res <- batch_upload_low_dimensional_data(jobs, etl_path = ETL_PATH,
  host = HOST, merge = 'REPLACE')
cat(tail(res$output), sep = '\n')

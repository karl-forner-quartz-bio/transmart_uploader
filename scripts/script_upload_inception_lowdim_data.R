# upload flowcyto data from PreciseSads clinical DB
###############################################################################

library(devtools)
library(RSQLite)
load_all('TransmartUploader')

################## PARAMETERS ##############################################
DB_ROOT <- 'db'
HOST <- 'pyro'
COHORT <- 'inception'
STUDY_ID <- 'AD2'
ETL_PATH <-  'Inception/Low Dimensional Data'
MERGE <- 'UPDATE_VARIABLES'

############### functions #################################################

db_connect <- function(path) {
  dbConnect(RSQLite::SQLite(), file.path(DB_ROOT, path))
}

upload <- function(df, node, merge = MERGE, ...) {
  # remove OMICID
  df$OMICID <- NULL
  df <- format_input_data(df, STUDY_ID)
  tt <- system.time(
    res <- upload_low_dimensional_data(df, ETL_PATH, node,
           merge = merge, host = HOST,  ...))
  cat('uploaded', node, 'in', tt[3], 's\n')
  print(head(res$stats))

  invisible(res)
}


#################### ANTIBODY ################################################
db <- db_connect('db_autoantibody_transmart.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'final_ki'))

res <- upload(df, 'Antibody')

#################### AUTO-ANTIBODY #############################################
db <- db_connect('db_autoantibody_transmart.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'final_ubo'))

res <- upload(df, 'Autoantibody')

#################### LUMINEX #############################################
db <- db_connect('db_luminex.db')
df <- dbReadTable(db, paste0(COHORT, '_', 'luminex'))

res <- upload(df, 'Luminex')

#################### HLA ###################################################
db <- db_connect('db_hla.db')
### ALLELES
df <- dbReadTable(db, paste0(COHORT, '_', 'hla_alleles'))
res <- upload(df, 'HLA+Alleles')

### INDELS
df <- dbReadTable(db, paste0(COHORT, '_', 'hla_indels'))
res <- upload(df, 'HLA+Indels')

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

res <- upload(df, 'Dummy', mapping = mapping)


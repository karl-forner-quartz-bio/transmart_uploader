library(devtools)
document('TransmartUploader')
load_all('TransmartUploader')

test('TransmartUploader')
test('TransmartUploader', 'sample')
test('TransmartUploader', 'fam')
test('TransmartUploader', 'map')
test('TransmartUploader', 'etl')



study_type <- "inception"
DB <- 'db/db_clinical.db'
res <- read_db(DB, study_type)

res <- upload_ecrfs_data(DB, study_type)
df<-res[[1]]
map_data <-res[[2]]
study_id <- res[[3]]
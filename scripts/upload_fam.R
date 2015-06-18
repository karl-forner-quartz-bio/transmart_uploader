
library(devtools)
devtools::load_all('TransmartUploader')

path <- "test_data/fam_files"

files <- dir(path, full.names = TRUE)
dfs <- lapply(files, read.table, header = F, check.names = FALSE, stringsAsFactors = FALSE)
names(dfs) <- basename(files)


dfs <- lapply(dfs, read_fam)


etl_path <- 'Sep/Test_Upload_Fam'
res<- upload_clinical_data(dfs, transmart_path= etl_path, study_id = "TEST_Fam_1", tissue_type = "Blood")



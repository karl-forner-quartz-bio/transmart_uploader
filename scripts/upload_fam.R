library(devtools)
devtools::load_all('TransmartUploader')

### simple upload
path <- 'Karl/Test_Upload_Fam'
upload_fam(fetch_ref_fam(), study_id = "ref",
  transmart_path = path, tissue_type = "Blood")
# res <- delete_study_by_id('ref')
# res <- delete_study_by_path('\\Karl\\Test_Upload_Fam\\')


res <- delete_study_by_path(path)

### low-level upload: messing up with dataa

fam <- read_fam(fetch_ref_fam())
# hack it a little
fam[10:20, 6] <- 1
fam[50:70, 6] <- 2
fam[101:110, 5] <- 0
fma_df <- format_fam(fam)
etl_path <- 'Karl/Test_Upload_Fam'
res <- upload_clinical_data(fma_df, transmart_path= etl_path, study_id = "ref2",
  tissue_type = "Blood")
# delete_study_by_id('ref2')

path <- "test_data/fam_files"

files <- dir(path, full.names = TRUE)
dfs <- lapply(files, read.table, header = F, check.names = FALSE, stringsAsFactors = FALSE)
names(dfs) <- basename(files)


dfs <- lapply(dfs, format_fam)


etl_path <- 'Sep/Test_Upload_Fam'
res<- upload_clinical_data(dfs, transmart_path= etl_path, study_id = "TEST_Fam_1", tissue_type = "Blood")



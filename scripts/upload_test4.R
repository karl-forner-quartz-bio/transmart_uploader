

devtools::load_all('TransmartUploader')
data <- fetch_test4_sample_data()
DATA_DFS <- data[c("Test4_1.txt", "Test4_2.txt")]
map <- data[["Test4_Mapping_File.txt"]]
map_df <- map[, c('category_cd', 'data_label')]
MAP_DF <- unique(map_df)


dir <- 'ETL_TEST'
# unlink(dir, recursive = TRUE)
dir.create(dir)
etl_path <- 'Toto/titi/tutu/This is a Test/ClinicalDataToUpload'
system.time(res <- run_tm_etl(DATA_DFS, MAP_DF, etl_path, dir = dir))
#    user  system elapsed
#   9.220   0.292   5.471



# delete_study_by_id('TEST4')



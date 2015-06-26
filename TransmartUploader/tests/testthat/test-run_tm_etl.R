context('run_tm_etl')

data <- fetch_test4_sample_data()
DATA_DFS <- data[c("Test4_1.txt", "Test4_2.txt")]
map <- data[["Test4_Mapping_File.txt"]]
map_df <- map[, c('category_cd', 'data_label')]
MAP_DF <- unique(map_df)

### tests not requiring the DB, coz nothing to or error


.run_tm_etl_no_study_id <- function() {
  expect_error(run_tm_etl(iris, MAP_DF, etl_path = 'Toto/XToUpload'),
    "STUDY_ID is MANDATORY")
}
test_that('run_tm_etl_no_study_id', .run_tm_etl_no_study_id())



# if the etl_path does not end with xxxToUpload nothing should happen
.run_tm_etl_bad_dir <- function() {
  res <- run_tm_etl(DATA_DFS, MAP_DF, etl_path = 'Toto/This is a Test')
  expect_null(res)
}
test_that('run_tm_etl_bad_dir', .run_tm_etl_bad_dir())




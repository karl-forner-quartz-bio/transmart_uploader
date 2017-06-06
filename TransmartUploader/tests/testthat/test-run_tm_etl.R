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



# run on "Test Studies/Low Dimentional Serial Data Test"
.just_run_tm_etl <- function() {
  just_run_tm_etl <- TransmartUploader:::just_run_tm_etl


  db <- requires_db()

  test_dir <- fetch_tMDataLoader_samples()
  setup_temp_dir()
  path <- file.path(test_dir,
    "Test Studies/Low Dimentional Serial Data Test/ClinicalDataToUpload")


  # read test data
  data_df <- read.table(file.path(path, "LDDTest.txt"),
    header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
  map_df <- read.table(file.path(path, "LDDTest_Mapping_File.txt"),
    header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
  browser()

  res <- just_run_tm_etl(list(data_df), map_df,
    etl_path = "Test Studies/Low Dimentional Serial Data Test/ClinicalDataToUpload",
    host = db$host, port= db$port, dir = '.')

  res <- just_run_tm_etl(list(data_df), map_df, etl_path = 'Toto/This is a Test',
    host = db$host, port= db$port)
  expect_null(res)
}
test_that('just_run_tm_etl', .just_run_tm_etl())
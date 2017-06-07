context('run_tm_etl')



# run on "Test Studies/Low Dimentional Serial Data Test"
.run_tm_etl_on_processed_data <- function() {
  run_tm_etl_on_processed_data <- TransmartUploader:::run_tm_etl_on_processed_data


  db <- requires_db()

  test_dir <- fetch_tMDataLoader_samples()
  setup_temp_dir()
  path <- file.path(test_dir,
    "Test Studies/Low Dimentional Serial Data Test/ClinicalDataToUpload")


  # read test data
  data_df <- read.table(file.path(path, "LDDTest.txt"),
    header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
  map_df <- read.table(file.path(path, "LDDTest_Mapping_File.txt"),
    colClasses = 'character',
    header = TRUE, sep = "\t",  stringsAsFactors = FALSE)

  etl_root <- "TransmartUploaderTest Studies"
  etl_path <-  file.path(etl_root, "Low Dim/ClinicalDataToUpload")

  delete_study_by_path(etl_root, host = db$host, port= db$port)

  res <- run_tm_etl_on_processed_data(data_df, map_df, etl_path = etl_path,
    host = db$host, port= db$port, dir = '.')

  expect_match(tail(res$output, 1), 'COMPLETED')

  expect_is(res$stats, 'data.frame')
  # -1 for STUDY_ID
  expect_equal(nrow(res$stats), length(data_df) - 1)
}
test_that('run_tm_etl_on_processed_data', .run_tm_etl_on_processed_data())



### tests not requiring the DB, coz nothing to do or error
.run_tm_etl_no_study_id <- function() {
  expect_error(run_tm_etl(iris, MAP_DF, etl_path = 'Toto/XToUpload'),
    "STUDY_ID is MANDATORY")
}
test_that('run_tm_etl_no_study_id', .run_tm_etl_no_study_id())



# if the etl_path does not end with xxxToUpload nothing should happen
.run_tm_etl_bad_dir <- function() {
  res <- run_tm_etl(DATA_DFS, MAP_DF, etl_path = 'Toto/This is a Test')
  expect_match(tail(res$output, 1), 'COMPLETED')
  expect_null(res$stats)
}
test_that('run_tm_etl_bad_dir', .run_tm_etl_bad_dir())




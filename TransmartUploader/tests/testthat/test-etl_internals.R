context('tm_etl_internals')


data <- fetch_test4_sample_data()
DATA_DFS <- data[c("Test4_1.txt", "Test4_2.txt")]
map <- data[["Test4_Mapping_File.txt"]]
map_df <- map[, c('category_cd', 'data_label')]
MAP_DF <- unique(map_df)




.write_etl_data_file <- function() {
  write_etl_data_file <- TransmartUploader:::write_etl_data_file
  setup_temp_dir()

  df <- head(mtcars)
  write_etl_data_file(df, 'toto.txt')

  df2 <- read.table('toto.txt', header = TRUE, sep = "\t", check.names = FALSE,
    stringsAsFactors = FALSE)

  expect_equivalent(df2, df)

  # add some NAs
  df[1, 5] <- NA
  df[5, 2] <- NA

  write_etl_data_file(df, 'toto.txt')
  df2 <- read.table('toto.txt', header = TRUE, sep = "\t", colClasses = 'character',
    stringsAsFactors = FALSE)
  expect_identical(df2[1, 5], '')
  expect_identical(df2[5, 2], '')

  df2 <- read.table('toto.txt', header = TRUE, sep = "\t",
    stringsAsFactors = FALSE, check.names = FALSE)

  expect_equivalent(df2, df)
}
test_that('write_etl_data_file', .write_etl_data_file())



.write_etl_mapping_file <- function() {
  setup_temp_dir()

  path <- 'map.txt'
  TransmartUploader:::write_etl_mapping_file(MAP_DF, path)

  expect_true(file.exists(path))
  df <- read.table(path, header = TRUE, sep = '\t', stringsAsFactors = FALSE)
  expect_equivalent(df, MAP_DF)

  ### with a merge mode
  TransmartUploader:::write_etl_mapping_file(MAP_DF, path, merge = 'REPLACE')
  expect_identical(readLines(path, 1), "#MERGE_MODE: REPLACE")

  df <- read.table(path, skip = 1, header = TRUE, sep = '\t', stringsAsFactors = FALSE)
  expect_equivalent(df, MAP_DF)
}
test_that('write_etl_mapping_file', .write_etl_mapping_file())



.write_etl_files <- function() {
  setup_temp_dir()

  dfs <- lapply(DATA_DFS, TransmartUploader:::format_input_data)
  fns <- sprintf('prefix_%i.txt', seq_along(dfs))
  map <- TransmartUploader:::generate_mapping(dfs, MAP_DF, fns)

  TransmartUploader:::write_etl_files(dfs, map, 'ETL', 'toto/titi/tutu', 'prefix')

  files <- dir('ETL/toto/titi/tutu')
  expect_identical(sort(files),
    c("prefix_1.txt", "prefix_2.txt", "prefix_Mapping_File.txt"))

}
test_that('write_etl_files', .write_etl_files())



.execute_etl_cmd <- function() {
  db <- requires_db()
  # N.B: upload_tMDataLoader_sample directly uses execute_etl_cmd
  upload_tMDataLoader_sample <- TransmartUploader:::upload_tMDataLoader_sample

  etl_path <- STUDIES
  sample_dir <- "Test Studies/Low Dimentional Serial Data Test"
  study_id <- 'LDDTest'

  delete_study_by_path(STUDIES, host = db$host, port = db$port)
  res <- upload_tMDataLoader_sample(sample_dir, etl_path, study_id,
    host = db$host, port = db$port)

  expect_match(res, 'MSG Procedure completed successfully', all = FALSE)

  delete_study_by_path(STUDIES, host = db$host, port = db$port)
}

test_that('execute_etl_cmd', .execute_etl_cmd())





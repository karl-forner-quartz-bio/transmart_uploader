context('tm_etl_internals')


data <- fetch_test4_sample_data()
DATA_DFS <- data[c("Test4_1.txt", "Test4_2.txt")]
map <- data[["Test4_Mapping_File.txt"]]
map_df <- map[, c('category_cd', 'data_label')]
MAP_DF <- unique(map_df)


.format_data_for_tmdataloader <- function() {
  df <- iris
  df$SUBJ_ID <- 1:nrow(df)
  df$toto <- 2
  df$STUDY_ID <- "coucou"

  map <- data.frame(
    data_label = c(names(iris), c('STUDY_ID', 'SUBJ_ID'))
    , stringsAsFactors = FALSE)
  map$category_cd <- 'dummy'

  df2 <- TransmartUploader:::format_data_for_tmdataloader(df, map)
  expect_identical(names(df2)[1:2], c('STUDY_ID', 'SUBJ_ID'))
  expect_identical(sort(names(df2)), sort(map$data_label))

  expect_true(all(names(df2) %in% map$data_label))
}
test_that('format_data_for_tmdataloader', .format_data_for_tmdataloader())



.build_tmdataloader_mapping_file <- function() {


  dfs <- lapply(DATA_DFS, TransmartUploader:::format_data_for_tmdataloader,
     MAP_DF)

  fns <- sprintf('data%i.txt', seq_along(dfs))
  res <- TransmartUploader:::build_tmdataloader_mapping_file(dfs, MAP_DF,
    fns)

  expect_true(is.data.frame(res))

  all_cols <- sort(unique(unlist(lapply(dfs, names), use.names = FALSE)))

  expect_identical(sort(unique(res$data_label)), all_cols)

  expect_identical(names(res),
     c('filename',	'category_cd', 	'col_nbr', 'data_label'))

}
test_that('build_tmdataloader_mapping_file', .build_tmdataloader_mapping_file())



.setup_etl_files <- function() {
  dir <- tempfile()
  dir.create(dir)
  old <- setwd(dir)
  on.exit({setwd(old); unlink(dir, recursive = TRUE)}, add = TRUE)

  dfs <- lapply(DATA_DFS, TransmartUploader:::format_data_for_tmdataloader,
    MAP_DF)
  fns <- sprintf('prefix_%i.txt', seq_along(dfs))
  map <- TransmartUploader:::build_tmdataloader_mapping_file(dfs, MAP_DF,
    fns)

  TransmartUploader:::setup_etl_files(dfs, map, 'ETL', 'toto/titi/tutu', 'prefix')

  files <- dir('ETL/toto/titi/tutu')
  expect_identical(sort(files),
    c("prefix_1.txt", "prefix_2.txt", "prefix_Mapping_File.txt"))

}
test_that('setup_etl_files', .setup_etl_files())



.run_etl_command <- function() {
  run_etl_command <- TransmartUploader:::run_etl_command

  db <- requires_db()


  test_dir <- normalizePath('tMDataLoader-samples')
  setup_temp_dir()

  dir <- "Test Studies/Low Dimentional Serial Data Test"

  etl_test <- 'ETL/TransmartUploaderTest  Studies'
  dir.create(etl_test, recursive = TRUE)

  file.copy(file.path(test_dir, dir), etl_test, recursive = TRUE)

  TransmartUploader:::create_etl_config('Config.groovy', host = db$host, port = db$port,
    data_dir = 'ETL')

  out <- run_etl_command(host = db$host, port = db$port, data_dir = 'ETL', dir = '.')

  expect_match(tail(out, 1), 'COMPLETED')

  # cleanup DB
  delete_study_by_path(dir, host = db$host, port = db$port)
}

test_that('run_etl_command', .run_etl_command())





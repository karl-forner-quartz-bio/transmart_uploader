context('batch jobs uploading')

..compute_sample_dfs <- function() {
  res <- fetch_test4_sample_data()
  df <- res[[2]]
  # adding SUBJ_ID and VISIT_NAME

  df$STUDY_ID <- NULL
  df2 <- df
  df2$Serum2 <- df2$Serum
  df2$`Cell Type` <- NULL
  df$SAMPLE_ID <- NULL
  df2$`Serum CRP Level, mg/dl` <- NULL
  df$toto <- 'toto'

  list(df1 = df, df2 = df2)
}


.multiple_upload_job <- function() {
  dfs <- ..compute_sample_dfs()

  job <- multiple_upload_job(dfs, study_id = 'TEST_BULK',
    categories = c('Clinical+DF1', 'Clinical+Labs+DF2'),
    keep = list(df2 = 'Blood'))

  df <- job[[1]]
  expect_is(df, 'data.frame')
  cols <- sort(colnames(df))
  all_cols <- sort(unique(unlist(lapply(dfs, colnames), use.names = FALSE)))

  # duplicated column is the only discarded
  expect_identical(setdiff(all_cols, cols), 'Serum')

  mapping <- job[[2]]
  expect_is(mapping, 'data.frame')
  expect_identical(names(mapping),
    c("filename", "category_cd", "col_nbr", "data_label"))

  expect_true(all(cols %in% mapping$data_label))

  expect_equal(mapping$col_nbr, seq_along(cols))
}
test_that('multiple_upload_job', .multiple_upload_job())




.upload_multiple_upload_job <- function() {
  db <- requires_db()

  dfs <- ..compute_sample_dfs()

  job <- multiple_upload_job(dfs, study_id = 'TEST_BULK',
    categories = c('Clinical+DF1', 'Clinical+Labs+DF2'),
    keep = list(df2 = 'Blood'))

  res <- upload_low_dimensional_data(job[[1]],
    etl_path = file.path(STUDIES, 'bulk_test'),
    mapping = job[[2]], host = db$host, port = db$port)

  expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)

  delete_study_by_id('TEST_BULK', host = db$host, port = db$port)
}
test_that('upload_multiple_upload_job', .upload_multiple_upload_job())


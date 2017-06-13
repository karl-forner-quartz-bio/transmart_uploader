context('uploading Low Dimensional data')


.bulk_upload_low_dimensional_data <- function() {
  db <- requires_db()
  setup_temp_dir()

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

  res <- bulk_upload_low_dimensional_data(list(df1 = df, df2 = df2), study_id = 'TEST_BULK',
    categories = c('Clinical+DF1', 'Clinical+Labs+DF2'),
    etl_path = file.path(STUDIES, 'bulk_test'),
    keep = list(df2 = 'Blood'),
     host = db$host, port = db$port)

   expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)

  delete_study_by_id('TEST_BULK', host = db$host, port = db$port)
}
test_that('bulk_upload_low_dimensional_data', .bulk_upload_low_dimensional_data())


# run on "Test Studies/Low Dimentional Serial Data Test"
.upload_low_dimensional_data_merge <- function() {
  db <- requires_db()

  setup_temp_dir()

  df <- head(iris)
  etl_path <- file.path(STUDIES, 'iris')

  df$SUBJ_ID <- paste0('ID', 1:nrow(df))
  df <- format_input_data(df, study_id = 'iris', tissue_type = 'blood')

  # delete left-over if any
  delete_study_by_path(etl_path, host = db$host, port = db$port)

  res <- upload_low_dimensional_data(df, etl_path, 'Subjects+Demographics',
    host = db$host, port = db$port)

  ### now test the UPDATE_VARIABLE mode
  df2 <- df[, 1:3]
  # change existing variable
  df2$Species <- LETTERS[1:nrow(df2)]
#  # add variable
  df2$toto <- 1:nrow(df2)
  res <- upload_low_dimensional_data(df2, etl_path, 'Subjects+Demographics',
    merge = 'UPDATE_VARIABLES',
    host = db$host, port = db$port)

  expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)
  expect_equal(unique(res$stats$N), nrow(df))

  delete_study_by_path(etl_path, host = db$host, port = db$port)
}
test_that('upload_low_dimensional_data_merge', .upload_low_dimensional_data_merge())



.upload_ACGHTEST_CLINICAL <- function() {
  db <- requires_db()
  add_categories <- TransmartUploader:::add_categories



  ### now upload it via upload_low_dimensional_data
  path <- file.path(TransmartUploader:::fetch_acgh_cnv_clinical_sample(),
    'ClinicalData/ACGHTEST_Clinical_data.txt')
  df <- read.table(path, sep = "\t", header = TRUE, check.names = FALSE,
    stringsAsFactors = FALSE)

  study_id <- 'ACGHTEST2'
  df$STUDY_ID <- study_id
  etl_path <- file.path(STUDIES, study_id)

  categ <- add_categories(
    c("lymphatic_invasion", "tumor_residual_disease", "venous_invasion",
      "Atomic_neoplasm_subdivision", "neoplasm_histologic_grade"),
       'Subjects+Medical_History+Tumor_Assessment')

  categ <- add_categories("person_neoplasm_cancer_status",
    'Clinical_Data+Clinical_Endpoints', categ)

  categ <- add_categories(c("histological_type"), "Subjects+Medical_History", categ)

  res <- upload_low_dimensional_data(df, etl_path, categ = categ,
    host = db$host, port = db$port)
  expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)

  expect_equal(unique(res$stats$N + res$stats$null), nrow(df))

 ### this part was useful for manual comparison between the two
#  ### now upload directly via existing sample files
#  study_id <- 'ACGHTEST'
#  etl_path <- file.path(STUDIES, study_id)
#
#  delete_study_by_path(STUDIES, host = db$host, port = db$port)
#
#  res <- upload_tMDataLoader_sample(sample_dir, etl_path, study_id,
#    host = db$host, port = db$port)
#
#  expect_match(res, 'MSG Procedure completed successfully', all = FALSE)


  delete_study_by_path(STUDIES, host = db$host, port = db$port)

}
test_that('upload_ACGHTEST_CLINICAL', .upload_ACGHTEST_CLINICAL())


# run on "Test Studies/Low Dimentional Serial Data Test"
.upload_low_dimensional_data <- function() {
  db <- requires_db()

  setup_temp_dir()

  df <- head(iris)
  etl_path <- file.path(STUDIES, 'iris')

  df$SUBJ_ID <- paste0('ID', 1:nrow(df))
  df <- format_input_data(df, study_id = 'iris', tissue_type = 'blood')

  # delete left-over if any
  delete_study_by_path(etl_path, host = db$host, port = db$port)

  res <- upload_low_dimensional_data(df, etl_path, 'Subjects+Demographics',
    host = db$host, port = db$port)

  expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)
  expect_equal(unique(res$stats$N), nrow(df))

  delete_study_by_path(etl_path, host = db$host, port = db$port)
}
test_that('upload_low_dimensional_data', .upload_low_dimensional_data())




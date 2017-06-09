context('uploading clinical data')


# run on "Test Studies/Low Dimentional Serial Data Test"
.upload_clinical_data_merge <- function() {
  db <- requires_db()

  setup_temp_dir()

  df <- head(iris)
  etl_path <- file.path(STUDIES, 'iris')

  df$SUBJ_ID <- paste0('ID', 1:nrow(df))
  df <- format_input_data(df, study_id = 'iris', tissue_type = 'blood')

  # delete left-over if any
  delete_study_by_path(etl_path, host = db$host, port = db$port)

  res <- upload_clinical_data(df, etl_path, 'Subjects+Demographics',
    host = db$host, port = db$port)

  ### now test the UPDATE_VARIABLE mode
  df2 <- df[, 1:3]
  # change existing variable
  df2$Species <- LETTERS[1:nrow(df2)]
#  # add variable
  df2$toto <- 1:nrow(df2)
  res <- upload_clinical_data(df2, etl_path, 'Subjects+Demographics',
    merge = 'UPDATE_VARIABLES',
    host = db$host, port = db$port)

  expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)
  expect_equal(unique(res$stats$N), nrow(df))

  delete_study_by_path(etl_path, host = db$host, port = db$port)
}
test_that('upload_clinical_data_merge', .upload_clinical_data_merge())



.upload_ACGHTEST_CLINICAL <- function() {
  db <- requires_db()
  # N.B: upload_tMDataLoader_sample directly uses execute_etl_cmd
  upload_tMDataLoader_sample <- TransmartUploader:::upload_tMDataLoader_sample
  add_categories <- TransmartUploader:::add_categories

  sample_dir <- "Test Studies/Test aCGH Copy Number Variations/ClinicalDataToUpload"

  ### now upload it via upload_clinical_data
  fname <- 'ACGHTEST_Clinical_data.txt'
  path <- file.path(fetch_tMDataLoader_samples(), sample_dir, fname)
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

  res <- upload_clinical_data(df, etl_path, categ = categ,
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
.upload_clinical_data <- function() {
  db <- requires_db()

  setup_temp_dir()

  df <- head(iris)
  etl_path <- file.path(STUDIES, 'iris')

  df$SUBJ_ID <- paste0('ID', 1:nrow(df))
  df <- format_input_data(df, study_id = 'iris', tissue_type = 'blood')

  # delete left-over if any
  delete_study_by_path(etl_path, host = db$host, port = db$port)

  res <- upload_clinical_data(df, etl_path, 'Subjects+Demographics',
    host = db$host, port = db$port)

  expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)
  expect_equal(unique(res$stats$N), nrow(df))

  delete_study_by_path(etl_path, host = db$host, port = db$port)
}
test_that('upload_clinical_data', .upload_clinical_data())




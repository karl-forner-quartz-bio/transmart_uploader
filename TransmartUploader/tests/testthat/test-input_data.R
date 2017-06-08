context('input data')



.format_input_data <- function() {
  format_input_data <- TransmartUploader:::format_input_data

  df <- head(iris)

  expect_error(format_input_data(df), 'STUDY_ID')
  expect_error(format_input_data(df, study_id = 'toto'), 'SUBJ_ID')

  df$SUBJ_ID <- paste0('ID', 1:nrow(df))


  df2 <- format_input_data(df, study_id = 'toto')
  expect_identical(names(df2)[1:2], c('STUDY_ID', 'SUBJ_ID'))
  expect_identical(df2[, -(1:2)], head(iris))

  df$STUDY_ID <- 'toto'
  df2 <- format_input_data(df, tissue_type = 'plasma')

  expect_identical(names(df2)[1:3], c('STUDY_ID', 'SUBJ_ID', 'Tissue_Type'))
  expect_identical(df2[, -(1:3)], head(iris))

}
test_that('format_input_data', .format_input_data())


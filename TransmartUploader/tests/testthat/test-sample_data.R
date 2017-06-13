context('sample data')


.fetch_test4_sample_data <- function() {
  data <- fetch_test4_sample_data()
  expect_true(is.list(data))
  expect_named(data, c("Test4_1.txt", "Test4_2.txt", "Test4_Mapping_File.txt"))

  expect_true(all(sapply(data, is.data.frame)))
}
test_that('fetch_test4_sample_data', .fetch_test4_sample_data())



.fetch_lowdimserialdata_sample <- function() {
  path <- TransmartUploader:::fetch_lowdimserialdata_sample()
  expect_true(dir.exists(path))
  expect_true(dir.exists(file.path(path, 'ClinicalData')))
}
test_that('fetch_lowdimserialdata_sample', .fetch_lowdimserialdata_sample())



.fetch_acgh_cnv_clinical_sample <- function() {
  path <- TransmartUploader:::fetch_acgh_cnv_clinical_sample()
  expect_true(dir.exists(path))
  expect_true(dir.exists(file.path(path, 'ClinicalData')))
}
test_that('fetch_acgh_cnv_clinical_sample', .fetch_acgh_cnv_clinical_sample())

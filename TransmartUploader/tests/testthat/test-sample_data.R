context('sample data')


.fetch_test4_sample_data <- function() {
  data <- fetch_test4_sample_data()
  expect_true(is.list(data))
  expect_named(data, c("Test4_1.txt", "Test4_2.txt", "Test4_Mapping_File.txt"))

  expect_true(all(sapply(data, is.data.frame)))

}
test_that('fetch_test4_sample_data', .fetch_test4_sample_data())




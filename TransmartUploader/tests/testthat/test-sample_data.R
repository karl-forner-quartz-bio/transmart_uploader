context('sample data')


.fetch_test4_sample_data <- function() {
  data <- fetch_test4_sample_data()
  expect_true(is.list(data))
  expect_named(data, c("Test4_1.txt", "Test4_2.txt", "Test4_Mapping_File.txt"))

  expect_true(all(sapply(data, is.data.frame)))
}
test_that('fetch_test4_sample_data', .fetch_test4_sample_data())



.fetch_tMDataLoader_samples <- function() {
  path <- fetch_tMDataLoader_samples()
  expect_true(dir.exists(path))

  expect_true(dir.exists(
      file.path(path, "Test Studies/Low Dimentional Serial Data Test")))
}
test_that('fetch_tMDataLoader_samples', .fetch_tMDataLoader_samples())

context('run_tm_etl')


.find_extdata_file <- function() {
  find_extdata_file <- TransmartUploader:::find_extdata_file

  path <- find_extdata_file('tm_etl.jar')
  expect_true(file.exists(path))

  expect_error(find_extdata_file('toto'), 'unable to find')

}
test_that('find_extdata_file', .find_extdata_file())



.check_jar <- function() {
  res <- check_jar('java', TransmartUploader:::find_extdata_file('tm_etl.jar'))
  expect_true(res)
}
test_that('check_jar', .check_jar())



.run_tm_etl <- function() {

}

test_that('run_tm_etl', .run_tm_etl())





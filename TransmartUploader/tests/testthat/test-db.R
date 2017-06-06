context('DB test')

.db_info <- function() {
  requires_db()

  db <- get_db()

  expect_true(nzchar(db$host))
  expect_true(is.numeric(db$port) && db$port > 0)
}
test_that('db_info', .db_info())

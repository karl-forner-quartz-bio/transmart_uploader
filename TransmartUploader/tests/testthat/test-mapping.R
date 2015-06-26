context('mapping')

.default_mapping <- function() {
  map <- default_mapping()
  expect_identical(names(map), c("category_cd", "data_label"))
  expect_true(nrow(map) > 4)
  expect_equivalent(sapply(map, class), c("character", "character"))
}
test_that('default_mapping', .default_mapping())



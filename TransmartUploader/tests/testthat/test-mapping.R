context('mapping')

.default_mapper <- function() {
  map <- default_mapper()
  expect_identical(names(map), c("category_cd", "data_label"))
  expect_true(nrow(map) > 4)
  expect_equivalent(sapply(map, class), c("character", "character"))
}
test_that('default_mapper', .default_mapper())



.generate_mapping <- function() {
  generate_mapping <- TransmartUploader:::generate_mapping

  dfs <- lapply(DATA_DFS, TransmartUploader:::format_data_for_tmdataloader,
    MAP_DF)

  fns <- sprintf('data%i.txt', seq_along(dfs))
  res <- generate_mapping(dfs, MAP_DF, fns)

  expect_true(is.data.frame(res))

  all_cols <- sort(unique(unlist(lapply(dfs, names), use.names = FALSE)))

  expect_identical(sort(unique(res$data_label)), all_cols)

  expect_identical(names(res),
    c('filename',	'category_cd', 	'col_nbr', 'data_label'))

}
test_that('generate_mapping', .generate_mapping())

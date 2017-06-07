context('mapping')

.base_mapper <- function() {
  map <- TransmartUploader:::base_mapper()

  expect_identical(names(map), c("category_cd", "data_label"))
  expect_true(nrow(map) == 3)
  expect_equivalent(sapply(map, class), c("character", "character"))
}
test_that('base_mapper', .base_mapper())



.default_mapper <- function() {
  map <- default_mapper()
  expect_identical(names(map), c("category_cd", "data_label"))
  expect_true(nrow(map) > 4)
  expect_equivalent(sapply(map, class), c("character", "character"))
}
test_that('default_mapper', .default_mapper())



.make_mapper <- function() {
  make_mapper <- TransmartUploader:::make_mapper

  expect_error(make_mapper(LETTERS[1:3], LETTERS[1:2]), 'bad arg "categories"')

  map_df <- make_mapper(LETTERS[1:3], rev(LETTERS[1:3]))

  df <- map_df[-(1:nrow(TransmartUploader:::base_mapper())), ]
  expect_identical(df[[1]], rev(LETTERS[1:3]))
  expect_identical(df[[2]], LETTERS[1:3])

  # recycling
  map_df <- make_mapper(LETTERS[1:3], 'toto')
  df <- map_df[-(1:nrow(TransmartUploader:::base_mapper())), ]
  expect_identical(unique(df[[1]]), 'toto')
  expect_identical(df[[2]], LETTERS[1:3])
}
test_that('make_mapper', .make_mapper())



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

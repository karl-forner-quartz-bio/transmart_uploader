context('mapping')


.base_categorization <- function() {
  categ <- TransmartUploader:::base_categorization()

  expect_identical(names(categ), c("category_cd", "data_label"))
  expect_identical(categ[[2]],
    c("STUDY_ID", "SUBJ_ID", "VISIT_NAME", "Age", "Sex", "Race", "Tissue_Type"))

  expect_equivalent(sapply(categ, class), c("character", "character"))
}
test_that('base_categorization', .base_categorization())



.add_categories <- function() {
  add_categories <- TransmartUploader:::add_categories

  expect_error(add_categories(LETTERS[1:3], LETTERS[1:2]), 'bad arg "categories"')

  map_df <- add_categories(LETTERS[1:3], rev(LETTERS[1:3]))

  df <- map_df[-(1:nrow(TransmartUploader:::base_categorization())), ]

  expect_identical(df[[1]], rev(LETTERS[1:3]))
  expect_identical(df[[2]], LETTERS[1:3])

  # recycling
  map_df <- add_categories(LETTERS[1:3], 'toto')
  df <- map_df[-(1:nrow(TransmartUploader:::base_categorization())), ]
  expect_identical(unique(df[[1]]), 'toto')
  expect_identical(df[[2]], LETTERS[1:3])
}
test_that('add_categories', .add_categories())



.simple_categorization <- function() {
  df <- DATA_DFS[[1]]
  base_categ <- TransmartUploader:::base_categorization()
  extra_vars <- setdiff(names(df), base_categ[[2]])

  categ <- simple_categorization(df)
  expect_true(all(!extra_vars %in% categ[[2]]))
  expect_identical(categ, base_categ)

  categ <- simple_categorization(df, 'Clinical')
  expect_true(all(names(df) %in% categ[[2]]))

  expect_identical(unique(categ[match(extra_vars, categ[[2]]), 1]), 'Clinical')
}
test_that('simple_categorization', .simple_categorization())




.build_mapping_file <- function() {
  build_mapping_file <- TransmartUploader:::build_mapping_file

  df <- DATA_DFS[[1]]
  categ <- TransmartUploader:::base_categorization()

  expect_error(build_mapping_file(iris, categ), 'mapping file is empty')

  map <- build_mapping_file(df, categ, 'titi.txt')

  expect_true(is.data.frame(map))

  expect_identical(sort(unique(map$data_label)), c("STUDY_ID", "SUBJ_ID"))
  expect_identical(names(map),
    c('filename',	'category_cd', 	'col_nbr', 'data_label'))
  expect_identical(unique(map$filename), 'titi.txt')

  expect_identical(names(df)[map$col_nbr], map$data_label)
  expect_identical(attr(map, 'merge'), 'REPLACE')

  categ2 <- rbind(categ,
    c('Subjects+Demographics', 'Type'),
    c('toto', 'Dataset'))

  map <- build_mapping_file(df, categ2, 'toto.txt', merge = 'UPDATE_VARIABLES')

  expect_identical(names(df)[map$col_nbr], map$data_label)
  expect_identical(attr(map, 'merge'), 'UPDATE_VARIABLES')
}
test_that('build_mapping_file', .build_mapping_file())



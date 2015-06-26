context('fam')


.read_fam <- function() {
  fam_path <- fetch_ref_fam()
  fam <- TransmartUploader:::read_fam(fam_path)
  expect_equal(dim(fam), c(1184, 6))
  expect_equivalent(sapply(fam, class),
    c("character", "character", "character", "character", "integer", "integer"))
}
test_that('read_fam', .read_fam())

.format_fam <- function() {
  fam <- TransmartUploader:::read_fam(fetch_ref_fam())


  # hack the data to exercise the function
  fam[10:20, 6] <- 1
  fam[50:70, 6] <- 2
  fam[101:110, 5] <- 0

  df <- TransmartUploader:::format_fam(fam)

  expect_identical(names(df), c("SUBJ_ID", "Sex", "Phenotype"))
  expect_equal(nrow(df), nrow(fam))

  # no more zero
  expect_false(any(df == 0, na.rm = TRUE))

  # subjids
  expect_equal(anyDuplicated(df$SUBJ_ID), 0)
  expect_identical(df$SUBJ_ID, fam[[2]])
  # Sex
  expect_true(all(df$Sex %in% c("Male", "Female", NA)))
  # pheno
  expect_true(all(df$Phenotype %in% c("Control", "Case", NA)))
}
test_that('format_fam', .format_fam())
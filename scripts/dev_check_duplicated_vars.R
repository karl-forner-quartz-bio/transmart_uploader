### TEST if a variable can be uploaded at multiple nodes ==> NO

DBHOST <- 'pyro'
DBPORT <- 5432

etl_root <- 'TransmartUploaderTest'
etl_path <-  file.path(etl_root, "Duplicates/ClinicalDataToUpload")

delete_study_by_path(etl_root, host = DBHOST)

df <- data.frame(
  STUDY_ID = 'toto',
  SUBJ_ID = 1:5,
  VISIT_NAME = 'M0',
  VAR = LETTERS[1:5],
  stringsAsFactors = FALSE
)
categ <- simple_categorization(df, 'VARS')
mapping <- build_mapping_file(df, categ)

res <- run_tm_etl_on_processed_data(df, mapping, etl_path = etl_path,
  host = DBHOST, port= DBPORT)

categ <- simple_categorization(df, 'Clinical+VAR')
mapping <- build_mapping_file(df, categ)
res <- run_tm_etl_on_processed_data(df, mapping, etl_path = etl_path,
  host = DBHOST, port= DBPORT, merge = 'UPDATE_VARIABLES')

expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)

### with duplicated line
df2 <- rbind(df[1,], df)
res <- run_tm_etl_on_processed_data(df2, mapping, etl_path = etl_path,
  host = db$host, port= db$port)
expect_match(res$output, 'MSG Procedure completed successfully', all = FALSE)

expect_is(res$stats, 'data.frame')
# -1 for STUDY_ID
expect_equal(nrow(res$stats), length(data_df) - 1)
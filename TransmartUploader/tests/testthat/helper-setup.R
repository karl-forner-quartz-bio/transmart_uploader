

get_db <- function(dbid = Sys.getenv('TRANSMART_DB')) {
  if (!nzchar(dbid)) return(NULL)
  items <- strsplit(dbid, '@', fixed = TRUE)[[1]]

  list(host = items[1], port = as.integer(items[2]))
}


requires_db <- function(db = get_db()) {
  if (is.null(db)) skip('This test requires a Transmart Test DB')

  db
}





upload_tMDataLoader_sample <- function(sample_dir, etl_path, study_id, ...) {
  setup_temp_dir()

  etl_test <- file.path('ETL', etl_path)
  dir.create(etl_test, recursive = TRUE)
  file.copy(sample_dir, etl_test, recursive = TRUE)


  create_etl_config('Config.groovy', data_dir = 'ETL', ...)

  execute_etl_cmd('Config.groovy')
}




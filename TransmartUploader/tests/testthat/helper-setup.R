

get_db <- function(dbid = Sys.getenv('TRANSMART_DB')) {
  if (!nzchar(dbid)) return(NULL)
  items <- strsplit(dbid, '@', fixed = TRUE)[[1]]

  list(host = items[1], port = as.integer(items[2]))
}


requires_db <- function(db = get_db()) {
  if (is.null(db)) skip('This test requires a Transmart Test DB')

  db
}









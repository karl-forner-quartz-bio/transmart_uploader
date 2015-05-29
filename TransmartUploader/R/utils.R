
find_extdata_file <- function(subpath) {
  path <- system.file(file.path('extdata', subpath), package = 'TransmartUploader')

  if (!nzchar(path) || !file.exists(path)) {
    stop(sprintf('unable to find extdata file "%s"', subpath))
  }

  path
}



check_jar <- function(java, jar) {
  args <- sprintf('-jar %s -h', jar)
  out <- system2(java, args, stdout = TRUE)
  grepl('usage', out[1])
}
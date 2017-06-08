
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

setup_temp_dir <- function(chdir = TRUE, ...) {
  dir <- tempfile(...)
  dir.create(dir, recursive = TRUE)
  old_dir <- NULL
  if (chdir) old_dir <- setwd(dir)

  # on one line because it not seen by the coverage
  cleanup <- bquote({if (.(chdir)) setwd(.(old_dir));unlink(.(dir), recursive = TRUE)})

  do.call(add_on_exit, list(cleanup, parent.frame()))

  invisible(normalizePath(dir))
}

add_on_exit <- function(expr, where = parent.frame()) {
  do.call("on.exit", list(substitute(expr), add = TRUE), envir = where)
}

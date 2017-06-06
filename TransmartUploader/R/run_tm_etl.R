

#' run the tMDataLoader tool
#'
#' @param data_dfs		the data to upload
#' @param map_df			the corresponding mapping
#' @param etl_path		the path in etl tree, as a standard path string
#' @param ...					additional arguments to \code{run_etl_command}
#'
#' @return the upload summary statistics as a data frame, or NULL
#'
#' @author karl
#' @export
run_tm_etl <- function(
  data_dfs,
  map_df,
  etl_path,
  ...
) {

  if (inherits(data_dfs, 'data.frame')) data_dfs <- list(data_dfs)

  # get study_id

  study_id <- unique(data_dfs[[1]]$STUDY_ID)
  if (is.null(study_id) || !nzchar(study_id)) {
    stop('STUDY_ID is MANDATORY')
  }

  # prepare data
  data_dfs <- lapply(data_dfs, format_data_for_tmdataloader, map_df)
  filenames <- sprintf('%s_%i.txt', study_id, seq_along(data_dfs))
  map_file_df <- build_tmdataloader_mapping_file(data_dfs, map_df, filenames)

  data_dir <- 'ETL'

  preprocess <- function() {
    setup_etl_files(data_dfs, map_file_df, data_dir, etl_path, study_id)
  }

  postprocess <- function() {
    fetch_etl_summary_statistics(file.path(data_dir, etl_path))
  }

  stats <- run_etl_command(data_dir = data_dir,
    preprocess = preprocess,
    postprocess = postprocess, ...)


  invisible(stats)
}








setup_etl_files <- function(data_dfs, map_df, data_dir, etl_path, prefix) {

  # make transmart path
  path <- file.path(data_dir, etl_path)
  dir.create(path, recursive = TRUE)

  # must be in the same order then dfs
  filenames <- file.path(path, unique(map_df$filename))
  for (i in seq_along(filenames)) {
    write.table(data_dfs[[i]], filenames[[i]],
      sep = '\t', quote = FALSE, row.names = FALSE)
  }

  map_path <- file.path(path, paste0(prefix, '_Mapping_File.txt'))
  write.table(map_df, map_path, sep = '\t', quote = FALSE, row.names = FALSE)

}




# this function reorders the df so that the first 2 columns
# are the STUDY_ID and SUBJ_ID, and remove unmapped columns
format_data_for_tmdataloader <- function(df, mapping_df) {
  STUDY_ID <- 'STUDY_ID'
  SUBJ_ID <- 'SUBJ_ID'

  if (!STUDY_ID %in% names(df)) stop("STUDY_ID column is missing")
  if (!SUBJ_ID %in% names(df)) stop("SUBJ_ID column is missing")

  # remove columns not in mapping
  cols <- intersect(names(df), mapping_df$data_label)
  df <- df[, cols]

  # reorder the columns
  mandatory <- c(STUDY_ID, SUBJ_ID)
  newcols <- c(mandatory, setdiff(names(df), mandatory))

  df <- df[, newcols]

  df
}



# generate the mapping for tMDataLoader as a data frame
# N.B: it is ordered in the same order than data_dfs
build_tmdataloader_mapping_file <- function(data_dfs, mapping_df, filenames) {
  vars <- mapping_df$data_label
  # 1. remove columns not in the mapping_df
  for (i in 1:length(data_dfs)) {
    cols <- intersect(names(data_dfs[[i]]), vars)
    data_dfs[[i]] <- data_dfs[[i]][, cols, drop = FALSE]
  }

  # make a subset of the mapping file
  cols <- unique(unlist(lapply(data_dfs, names), use.names = FALSE))
  map <- mapping_df[mapping_df$data_label %in% cols, ]


  # now create the tMDataLoader mapping file with columns filename and col_nbr
  .make_mapping <- function(i) {
    df <- data_dfs[[i]]
    mm <- data.frame(data_label = names(df), col_nbr = 1:ncol(df),
      stringsAsFactors = FALSE)
    res <- merge(mm, map, by = 'data_label')
    res$filename <- filenames[i]
    res <- res[, c('filename',	'category_cd', 	'col_nbr', 'data_label')]

    # order by col_nbr
    res[order(res$col_nbr), ]
  }

  res <- lapply(seq_along(data_dfs), .make_mapping)
  res <- do.call(rbind, res)

  res
}


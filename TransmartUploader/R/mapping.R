#' Generate the default map of variables to the TranSMART tree
#'
#' This mapper will be used to generate the mapping file, which is
#' mandatory to upload the data into tranSMART
#'
#' @return a data.frame with columns "category_cd" and "data_label"
#'
#' @author Sepideh
#' @export
default_mapper <- function() {

  col_path <- " / /Tissue/Subject+Demographic/Subject+Phenotype"
  col_name <- "STUDY_ID/SUBJ_ID/Tissue_Type/Sex/Phenotype"

  col_path <- unlist(strsplit(col_path, "/"))
  col_name <- unlist(strsplit(col_name, "/"))

  map_data <- data.frame(category_cd = col_path, data_label = col_name,
    stringsAsFactors = FALSE)

  map_data
}

#' generate a simple mapper
#'
#' @param base				the base mapper to use, for the uncategorized vars
#' 	such as STUDY_ID,...
#' @param vars				the var names to map as a character vector
#' @param categories	the categories as a character vector. Will be recycled if
#' 										of length 1, otherwise must have the same length as vars
#' @return a data.frame with columns "category_cd" and "data_label"
#' @author karl
#' @keywords internal
make_mapper <- function(vars, categories, base = base_mapper()) {
  if (length(categories) == 1) { # recycle
    categories <- rep(categories, length(vars))
  }

  if (length(categories) != length(vars)) stop('bad arg "categories"')

  rbind(base,
    data.frame(category_cd = categories, data_label = vars,
      stringsAsFactors = FALSE))
}



#' Generate the minimal map of variables to the TranSMART tree
#'
#' @return a data.frame with columns "category_cd" and "data_label"
#'
#' @author karl
#' @keywords internal
base_mapper <- function() {
  col_path <- " / /Tissue"
  col_name <- "STUDY_ID/SUBJ_ID/Tissue_Type"

  col_path <- unlist(strsplit(col_path, "/"))
  col_name <- unlist(strsplit(col_name, "/"))

  map_data <- data.frame(category_cd = col_path, data_label = col_name,
    stringsAsFactors = FALSE)

  map_data
}


#' generate the mapping file for tMDataLoader as a data frame
#'
#' N.B1: the data_dfs must be properly formatted
#'
#' N.B2: same order as data_dfs
#'
#' @inheritParams params
#' @param mapper	the mapper to use for generating the mapping file
#' @return the mapping as a data frame
#' @author karl
#' @keywords internal
generate_mapping <- function(data_dfs, mapper, filenames) {
  vars <- mapper$data_label
  # 1. remove columns not in the mapper
  for (i in 1:length(data_dfs)) {
    cols <- intersect(names(data_dfs[[i]]), vars)
    data_dfs[[i]] <- data_dfs[[i]][, cols, drop = FALSE]
  }

  # make a subset of the mapping file
  cols <- unique(unlist(lapply(data_dfs, names), use.names = FALSE))
  map <- mapper[mapper$data_label %in% cols, , drop = FALSE]

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

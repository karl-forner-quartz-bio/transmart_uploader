#' Mapping Concepts
#'
#' Let us clarify the different concepts related to mapping.
#'
#' \describe{
#' 		\item{ Mapping File }{ file used by tMDataLoader to upload data in the
#' 		 Transmart tree. It has 4 columns, and it is not user friendly to edit
#' 		since e.g. you have to indicate the column numbers and repeat the study name }
#'
#'		\item{ categorization }{ a concept used by TransmartUploader, it is
#' 			an association of variable names to a category code in the Transmart tree,
#'  		as a data frame with 2 columns.
#' 			It is the core part of a \code{Mapping File}, meant for humans, that
#' 			will eventually be used to generate the final \code{Mapping File}. }
#'
#' 	\item{ base categorization }{ rather mininimal and standard categorization
#' 		about uncategorized vars such as \code{STUDY_ID} and \code{SUBJ_ID}
#' 		and common vars such as \code{tissue_type}, cf \code{\link{base_categorization}}}
#'
#' 	\item{ applying a categorization to a data frame }{ cf \code{\link{build_mapping_file}} }
#'
#'
#' }
#'
#' @param df				a data frame meant to be uploaded at some point. Must contain a
#' 	a SUBJ_ID column
#' @param categ			a categorization as a data frame
#' @param mapping		the mapping file as a data frame
#' @param merge			the merge mode, cf
#' 	\url{https://github.com/Clarivate-LSPS/tMDataLoader/wiki/02.-Clinical-Data#merge-modes}
#'
#' @name mapping
NULL

#' base categorization
#'
#' This mapper will be used to generate the mapping file, which is
#' mandatory to upload the data into tranSMART
#'
#' @return a data.frame with columns "category_cd" and "data_label"
#'
#' @seealso mapping
#' @author karl
#' @keywords internal
base_categorization <- function() {
  col_path <- " / / /Subjects+Demographics/Subjects+Demographics/Subjects+Demographics/Tissue"
  col_name <- "STUDY_ID/SUBJ_ID/VISIT_NAME/Age/Sex/Race/Tissue_Type"

  col_path <- unlist(strsplit(col_path, "/"))
  col_name <- unlist(strsplit(col_name, "/"))

  map_data <- data.frame(category_cd = col_path, data_label = col_name,
    stringsAsFactors = FALSE)

  map_data
}

#' utility to make a categorization with a default category
#'
#' @inheritParams mapping
#' @param category	a default category to apply to \code{vars} if not null
#' @param vars							the vars on which to apply the default category
#' @return a data.frame with columns "category_cd" and "data_label"
#'
#' @seealso mapping
#' @author karl
#' @export
simple_categorization <- function(
  df,
  category = NULL,
  categ = base_categorization(),
  vars = setdiff(colnames(df), categ[[2]]))
{
  if (!is.null(category)) {
    categ <- add_categories(vars, category, categ)
  }

  categ
}


#' add categories to an existing categorization
#'
#' @param vars				the var names to map as a character vector
#' @param categories	the categories as a character vector. Will be recycled if
#' 										of length 1, otherwise must have the same length as vars
#' @param base				the base categorization to use
#' @return a categorization as a data frame
#' @author karl
#' @keywords internal
add_categories <- function(vars, categories, base = base_categorization()) {
  if (length(categories) == 1) { # recycle
    categories <- rep(categories, length(vars))
  }

  if (length(categories) != length(vars)) stop('bad arg "categories"')

  rbind(base,
    data.frame(category_cd = categories, data_label = vars,
      stringsAsFactors = FALSE))
}


#' categorization of multiple tables
#'
#' @inheritParams mapping
#' @param vars							the vars on which to apply the default category
#' @return a data.frame with columns "category_cd" and "data_label"
#'
#' @seealso mapping
#' @author karl
#' @keywords internal
multi_categorization <- function(
  data_dfs,
  categories,
  categ = base_categorization())
{
  base = base_categorization()
  null_categ <- base[NULL, ]

  categs <- mapply(simple_categorization, data_dfs, categories,
    MoreArgs = list(categ = null_categ), SIMPLIFY = FALSE)
  categ <- do.call(rbind.data.frame, categs)
  row.names(categ) <- NULL

  rbind(base, categ, stringsAsFactors = FALSE)
}


#' generate the mapping file for tMDataLoader as a data frame
#'
#' additional attributes such as the merge mode are stored as attributes
#'
#' @param filename	the filename part of the Mapping File, i.e. the
#' 	name of the data file for tMDataLoader
#' @inheritParams mapping
#' @return the mapping file as a data frame
#' @author karl
#' @export
build_mapping_file <- function(df, categ, filename = 'data.txt',
  merge = c('REPLACE', 'UPDATE', 'UPDATE_VARIABLES', 'APPEND'))
{
  merge <- match.arg(merge)

  ### fit categ to df
  ind <- na.omit(match(names(df), categ[[2]]))
  if (length(ind) == 0) stop('Error: mapping file is empty')

  map <- categ[ind, , drop = FALSE]

  # column numbers
  map$col_nbr <- match(map[[2]], names(df))
  map$filename <- filename

  # reorder columns
  map <- map[, c("filename", "category_cd", "col_nbr", "data_label")]

  # add merge mode
  attr(map, 'merge') <- merge

  map
}



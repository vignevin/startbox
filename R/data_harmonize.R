#' @title Harmonize Column Types
#'
#' @description
#' Harmonizes the types of columns in a dataframe according to a type mapping.
#'
#' @param df The dataframe to harmonize.
#' @param types_map (optional) predefined type mapping (otherwise it will load from a CSV).
#' @param dictionary_path (optional) path to the CSV dictionary file if types_map is not provided.
#'
#' @return A dataframe with harmonized column types.
#' @export
harmonize_column_types <- function(df, types_map = NULL, dictionary_path = system.file("extdata", "star_dictionary.csv", package = "startbox")) {

  if (is.null(types_map)) {
    types_df <- utils::read.csv2(dictionary_path, stringsAsFactors = FALSE)
    types_df <- types_df[!(is.na(types_df$nom) | types_df$nom == "" |
                             is.na(types_df$Rclass) | types_df$Rclass == ""), ]
    types_map <- stats::setNames(as.list(types_df$Rclass), types_df$nom)
  }

  for (col in names(types_map)) {
    if (col %in% names(df)) {
      type <- types_map[[col]]

      if (type == "date") {
        # Multiple attempts on common formats
        original_dates <- df[[col]]
        tryFormats <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y")
        success <- FALSE

        for (fmt in tryFormats) {
          test <- suppressWarnings(as.Date(original_dates, format = fmt))
          if (all(!is.na(test) | is.na(original_dates))) {
            df[[col]] <- test
            message(paste("Column", col, "converted with format", fmt))
            success <- TRUE
            break
          }
        }

        if (!success) {
          warning(paste("Could not convert column", col, "to Date. It remains as text."))
          df[[col]] <- as.character(original_dates)
        }

      } else {
        # Standard conversion
        df[[col]] <- switch(
          type,
          character = as.character(df[[col]]),
          numeric   = as.numeric(as.character(df[[col]])),
          integer   = as.integer(as.character(df[[col]])),
          df[[col]]
        )
      }
    }
  }

  return(df)
}

#' @title Harmonize All Observation Data Tables
#'
#' @description
#' Harmonizes all observation datasets using harmonize_column_types().
#'
#' @param obs_data A list of observation datasets.
#'
#' @return A list of harmonized observation datasets.
#' @export
harmonize_all_obs_data <- function(obs_data) {
  lapply(obs_data, harmonize_column_types)
}



#' Remove string in a character vector
#'
#' @description
#' This function takes two character vectors of the same length and, for each position, removes the string in \code{pattern[i]} from \code{vec[i]}, ignoring case.
#' If either element at position \code{i} equals "TNT" (case-insensitive), it returns "TNT" for that position instead.
#' @param vec a character vector
#' @param pattern a character vector
#' @param separator a character that separate parts of the string, for example "_"
#'
#' @returns a character vector
#' @export
#'
#' @examples
#' plot_id <- c("1a","2A","1B","tnt1","4c")
#' plot_block <- c("A","A","B","1","C")
#' remove_string_pairwise(vec = plot_id, pattern = plot_block)
remove_string_pairwise <- function(vec, pattern,separator=NULL) {
  # type validation
  if (!is.character(vec)) {
    stop("vec MUST be a character vector")
  }
  if (!is.character(pattern)) {
    stop("pattern MUST be a character vector")
  }

  if (length(vec) != length(pattern)) {
    stop("The 2 vectors MUST have the same length")
  }

  if (!is.null(separator)) {
    pattern <- paste0(separator,pattern)
  }

  result <- mapply(function(x, y) {
    if (grepl("TNT", x, ignore.case = TRUE) || grepl("TNT", y, ignore.case = TRUE)) {
      return("TNT")
    } else {
      return(gsub(y, "", x, ignore.case = TRUE))
    }
  }, vec, pattern, USE.NAMES = FALSE)
  print(paste("Strings extracted are :",paste(unique(result),collapse=" ; ")))
  return(result)
}

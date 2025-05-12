#' @description
#' Harmonizes the types of columns in a dataframe according to a type mapping.
#'
#' @param df The dataframe to harmonize.
#' @param types_map (optional) predefined type mapping (otherwise it will load from a CSV).
#' @param dictionary_path (optional) path to the CSV dictionary file if types_map is not provided.
#' @param format_date (optional) a character vector of date formats to try (e.g., c("%d/%m/%Y", "%Y-%m-%d")).
#'
#' @return A dataframe with harmonized column types.
harmonize_column_types <- function(df, types_map = NULL, dictionary_path = "inst/extdata/star_dictionary.csv") {
  
  if (is.null(types_map)) {
    types_df <- read.csv2(dictionary_path, stringsAsFactors = FALSE)
    types_df <- types_df[!(is.na(types_df$nom) | types_df$nom == "" |
                             is.na(types_df$Rclass) | types_df$Rclass == ""), ]
    types_map <- setNames(as.list(types_df$Rclass), types_df$nom)
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
            message(paste("✅ Column", col, "converted with format", fmt))
            success <- TRUE
            break
          }
        }
        
        if (!success) {
          warning(paste("❌ Could not convert column", col, "to Date. It remains as text."))
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

#' @description
#' Harmonizes all observation datasets using harmonize_column_types().
#'
#' @param obs_data A list of observation datasets.
#'
#' @return A list of harmonized observation datasets.
harmonize_all_obs_data <- function(obs_data) {
  lapply(obs_data, harmonize_column_types)
}







#' extract experimental traitement code from plot_id
#'
#' @description
#' This function remove the block code from a vector of class character.
#' It works well when block code are letters and experimental treatment code are numbers or vice versa.
#' TNT are directly recognized as TNT
#' @param pid a vector of character
#' @param blocks a vector of block codes (could be a pb if blocks code are a mix of letters and numbers...)
#' @param separator a character that separate block code from the other part of the string, for example "_"
#'
#' @returns the pid vector without blocks codes
#' @export
#'
#' @examples
#'
#' blocks=c("A","B","C","D")
#' pid=c("1A","1B","TNT3","2A","2D")
#' remove_block_code(pid=pid,blocks=blocks)
remove_block_code <- function(pid,blocks,separator=NULL) {
  # empty results vector with the same length as pid
  xp_trt_code <- vector(length = length(pid))
  # remove leading or trailing whitespace in case of
  pid <- trimws(pid)
  # identify all "TNT" in pid
  select_TNT <- grepl("^TNT", pid)
  # if element contains TNT, TNT is expected in the result vector
  xp_trt_code[select_TNT] <- "TNT"
  # f element does NOT contains, remove the block code
  xp_trt_code[!select_TNT] <- gsub(paste0("[",paste(blocks,collapse = ","),"]"), "", pid[!select_TNT])
  # remove the separator
  if (!is.null(separator)) {
    xp_trt_code[!select_TNT] <- gsub(separator, "", xp_trt_code[!select_TNT])
  }
  return(xp_trt_code)
}


#' Title
#'
#' @param data a dataframe to resume
#' @param var_col character, the colname of the variable to plot
#' @param group_cols the colname of the group
#' @param funs statistic to plot, a vector of one or two statistic. by default c("mean","frequency")
#'
#' @returns a dataframe ready for plotting
#' @export
#'
#' @examples
resume_data <- function(data, var_col, group_cols,funs = list(intensite=mean,frequence=frequency)) {
  
  # flag_variable
  flag_variable <- "variable" %in% colnames(data)
  
  # convert column argument to symbol
  var <- dplyr::ensym(var_col)
  if(flag_variable) group_syms <- dplyr::syms(c(group_cols,"variable")) else group_syms <- dplyr::syms(group_cols)
  
  data_resume <- data.frame()
  for (i in 1:length(funs))
  {
    if (identical(funs[[i]], base::mean)) {
      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(
          lower.CL = mean({{ var }}, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd({{ var }}, na.rm = TRUE) / sqrt(n()),
          upper.CL = mean({{ var }}, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd({{ var }}, na.rm = TRUE) / sqrt(n()),
          value = mean({{ var }}, na.rm = TRUE), # we need to do that after lower.CL and upper.CL calculation, else value = var
          .groups = "drop"
        ) %>%
        dplyr::mutate(type = if ("type" %in% colnames(data)) paste(type, !!names(funs)[i]) else !!names(funs)[i])
    } else {
      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(value = funs[[i]]({{ var }}), .groups = "drop") %>%
        dplyr::mutate(type = if ("type" %in% colnames(data)) paste(type, !!names(funs)[i]) else !!names(funs)[i])
    }
    data_resume <- dplyr::bind_rows(data_resume,resume)
    if(!flag_variable) data_resume$variable = var_col
  }
  return(data_resume)
} #end function
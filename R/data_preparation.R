## data preparation functions of startbox

#' @title Harmonize Column Types
#'
#' @description
#' Harmonizes the types of columns in a dataframe according to a type mapping.
#'
#' @param df The dataframe to harmonize.
#' @param dictionary (optional) a data.frame with a least 2 cols 'nom' and 'Rclass'
#'
#' @return A dataframe with harmonized column types.
#' @export
harmonize_column_types <- function(
  df,
  dictionary = NULL
 ) {
  if (is.null(dictionary)) {
    dictionary_path = system.file(
      "extdata",
      "star_dictionary.csv",
      package = "startbox"
    )
    if(!file.exists(dictionary_path)) {stop("no dictionary found")}
    types_df <- utils::read.csv2(dictionary_path, stringsAsFactors = FALSE)
     } else {
       types_df <- dictionary
     }

  types_df <- types_df[
    !(is.na(types_df$nom) |
        types_df$nom == "" |
        is.na(types_df$Rclass) |
        types_df$Rclass == ""),
  ]
  if (nrow(types_df) == 0) {stop("empty dictionary or missing data")}
  # type mapping
  types_map <- stats::setNames(as.list(types_df$Rclass), types_df$nom)

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
          warning(paste(
            "Could not convert column",
            col,
            "to Date. It remains as text."
          ))
          df[[col]] <- as.character(original_dates)
        }
      } else {
        # Standard conversion
        df[[col]] <- switch(
          type,
          character = as.character(df[[col]]),
          numeric = as.numeric(as.character(df[[col]])),
          integer = as.integer(as.character(df[[col]])),
          df[[col]]
        )
      }
    }
  }

  return(df)
}
#' @title Merged a dataset with plot description
#'
#' @description
#' Merges observation data with metadata sheets "placette" if available.
#' The function joins the 2 dataframes using the `plot_id` column.
#'
#' @param self An instance of the `UserData` R6 class containing metadata$plot_desc.
#' @param df a dataframe to join.
#' @param flex boolean. if TRUE, 10A A1O or 10a are considered as the same plot_id
#'
#' @return A `data.frame` with observation data enriched by plot and treatment metadata.
#' If no observation data is available, returns only the merged plot + modality metadata.
#' If required metadata is missing, returns `NULL` with a message.
#'
#' @details
#' - Assumes that `self$metadata$plot_desc` contains `plot_id`.
#'
#' @export
merge_data_plotdesc <- function(self, df, flex = F) {
  # check if self is UserData
  if (!inherits(self, "UserData")) {
    message(
      "Argument self is not an UserData object as required by the function"
    )
    return(NULL)
  }

  if (is.null(self$metadata$plot_desc)) {
    message("plot_desc missing.")
    return(NULL)
  }

  ## check if plot_id if in both dataframes
  if (
    !"plot_id" %in% intersect(colnames(self$metadata$plot_desc), colnames(df))
  ) {
    message("Col plot_id not in both dataframes")
    return(NULL)
  }

  ## plot_id HAS TO BE a character (it should)
  if (!is.character(self$metadata$plot_desc$plot_id)) {
    self$metadata$plot_desc$plot_id <- as.character(
      self$metadata$plot_desc$plot_id
    )
  }
  if (!is.character(df$plot_id)) {
    df$plot_id <- as.character(df$plot_id)
  }

  ##
  if (check_plotid_diff(self$metadata$plot_desc, df)) {
    message(
      "WARNING : at least one plot_id in the dataset was not found in the sheet placette (plot_desc in metadata)"
    )
  }

  if (flex) {
    message("flex merging for plot_id. 10A = A10 = a10")
    df$clean_id <- harmonize_plot_id_format(df$plot_id) # harmonize plot_id
    tmp_plot_desc <- self$metadata$plot_desc
    tmp_plot_desc$clean_id <- harmonize_plot_id_format(tmp_plot_desc$plot_id) # harmonize plot_id
    tmp_plot_desc <- tmp_plot_desc[, !names(tmp_plot_desc) %in% c("plot_id")] ## removing plot_id
    # join data and plot description
    df_plot_desc <- dplyr::left_join(
      df,
      tmp_plot_desc,
      by = "clean_id"
    )
  } else {
    # join data and plot description
    df_plot_desc <- dplyr::left_join(
      df,
      self$metadata$plot_desc,
      by = "plot_id"
    )
  }

  if (nrow(df_plot_desc) == 0) {
    message("no common plot_id !")
    return(NULL)
  }

  if (nrow(df_plot_desc) < nrow(df)) {
    message("WARNING : we lost data ! (unknown error)")
  }
  return(df_plot_desc)
}


#' @title Merged a dataset with experimental treatment description
#'
#' @description
#' Merges observation data with metadata sheets "modalite" if available.
#' The function joins the 2 dataframes using the `xp_trt_code` column.
#'
#' @param self An instance of the `UserData` R6 class containing metadata$moda_desc
#' @param df a dataframe to join.
#'
#' @return A `data.frame` with observation data enriched by ptreatment metadata.
#' If required metadata is missing, returns `NULL` with a message.
#'
#' @details
#' - Assumes that `self$metadata$moda_desc` contains `xp_trt_code`.
#'
#' @export
merge_data_xpdesc <- function(self, df) {
  # check if self is UserData
  if (!inherits(self, "UserData")) {
    message(
      "Argument self is not an UserData object as required by the function"
    )
    return(NULL)
  }

  if (is.null(self$metadata$moda_desc)) {
    message("moda_desc missing.")
    return(NULL)
  }

  ## check if plot_id if in both dataframes
  if (
    !"xp_trt_code" %in%
      intersect(colnames(self$metadata$moda_desc), colnames(df))
  ) {
    message("Col xp_trt_code not in both dataframes")
    return(NULL)
  }

  ## xp_trt_code HAS TO BE a character (it should)
  if (!is.character(self$metadata$moda_desc$xp_trt_code)) {
    self$metadata$moda_desc$xp_trt_code <- as.character(
      self$metadata$moda_desc$xp_trt_code
    )
  }
  if (!is.character(df$xp_trt_code)) {
    df$xp_trt_code <- as.character(df$xp_trt_code)
  }

  ##
  if (check_xptrtcode_diff(self$metadata$moda_desc, df)) {
    message(
      "WARNING : at least one xp_trt_code in the dataset was not found in the sheet modalite (moda_desc in metadata)"
    )
  }

  # join data and plot description
  df_moda_desc <- dplyr::left_join(
    df,
    self$metadata$moda_desc,
    by = "xp_trt_code"
  )
  if (nrow(df_moda_desc) == 0) {
    message("no common xp_trt_code !")
    return(NULL)
  }

  if (nrow(df_moda_desc) < nrow(df)) {
    message("WARNING : we lost data ! (unknown error)")
  }
  return(df_moda_desc)
}

#' @title Standardize a raw TopVigne CSV file
#'
#' @description
#' Checks that the file is a valid TopVigne CSV using `check_topvigne_csv()`,
#' then renames and reshapes the data using a lookup table and standard rules.
#'
#' @param filepath Character. Path to the raw .csv file to standardize.
#' @param lookup_table Named character vector. Mapping from original column names to standardized names.
#'
#' @return A standardized data.frame with consistent column names and structure.
#'
#' @export
standardise_topvigne_csv <- function(
  filepath,
  lookup_table = c(
    "Mildiou_Feuille" = "PM_LEAF_PC",
    "Mildiou_Grappe" = "PM_BER_PC",
    "Oidium_Grappe" = "UN_BER_PC",
    "Oidium_Feuille" = "UN_LEAF_PC",
    "Stade.phénologique" = "bbch_stage",
    "Stade.phenologique" = "bbch_stage",
    "Stade phenologique" = "bbch_stage",
    "Placette" = "plot_id",
    "Bloc" = "block_code",
    "Date" = "observation_date",
    "Code.essai" = "experiment_id"
  )
) {
  # local binding
  Maladie <- Organe <- Valeur <- bbch_stage <- NULL

  check_topvigne_csv(filepath)

  df <- utils::read.csv2(file = filepath, fileEncoding = "latin1")

  if (all(c("Maladie", "Organe", "Valeur") %in% names(df))) {
    df <- df %>%
      tidyr::pivot_wider(names_from = c(Maladie, Organe), values_from = Valeur)
  }

  noms_avant <- names(df)
  noms_apres <- ifelse(
    noms_avant %in% names(lookup_table),
    lookup_table[noms_avant],
    noms_avant
  )
  names(df) <- noms_apres

  if ("bbch_stage" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        bbch_stage = ifelse(
          grepl("^BBCH", bbch_stage),
          bbch_stage,
          paste("BBCH", bbch_stage)
        )
      )
  }

  colonnes_standard <- unname(lookup_table)
  colonnes_presentes <- names(df)
  colonnes_autres <- setdiff(colonnes_presentes, colonnes_standard)
  df <- df[, c(
    intersect(colonnes_standard, colonnes_presentes),
    colonnes_autres
  )]

  # Remove unwanted column "X." if still present
  df <- df[, !names(df) %in% c("X.", "X")]

  return(df)
}

#' Standardize an experimental data file or data frame
#'
#' @param input_data A path to a .csv or .xlsx file to import, or a data.frame already loaded in R.
#' @param lookup_table Optional. A named vector mapping original column names to standardized names. If NULL, a default mapping is used.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_detect str_extract
#' @importFrom readr read_csv2
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#'
#' @return A cleaned and structured data.frame, ready for use in downstream processing and analysis.
#' @export
#'
standardise_data <- function(
  input_data,
  lookup_table = c(
    "Mildiou_Feuille" = "PM_LEAF_PC",
    "Mildiou_Grappe" = "PM_BER_PC",
    "Oidium_Grappe" = "UN_BER_PC",
    "Oidium_Feuille" = "UN_LEAF_PC",
    "Stade.phénologique" = "bbch_stage",
    "Stade.phenologique" = "bbch_stage",
    "Stade phenologique" = "bbch_stage",
    "Placette" = "plot_id",
    "Bloc" = "block_id",
    "Date" = "observation_date",
    "Code.essai" = "experiment_id"
  )
) {
  # local binding
  Maladie <- Organe <- Valeur <- bbch_stage <- NULL

  if (is.data.frame(input_data)) {
    df <- input_data
  } else if (is.character(input_data)) {
    ext <- tools::file_ext(input_data)

    # read data
    if (ext == "csv") {
      df <- utils::read.csv2(file = input_data, fileEncoding = "latin1")
    } else if (ext == "xlsx") {
      df <- readxl::read_excel(input_data, sheet = "data")
    }
  } else {
    stop("non supported format. Use a .csv or .xlsx file")
  }

  # check cols presence
  if (all(c("Maladie", "Organe", "Valeur") %in% names(df))) {
    df <- df %>%
      tidyr::pivot_wider(names_from = c(Maladie, Organe), values_from = Valeur)
  }

  # rename cols according to lookup_table
  noms_avant <- names(df)
  noms_apres <- ifelse(
    noms_avant %in% names(lookup_table),
    lookup_table[noms_avant],
    noms_avant
  )
  names(df) <- noms_apres

  # format bbch
  if ("bbch_stage" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        bbch_stage = ifelse(
          grepl("^BBCH", bbch_stage),
          bbch_stage,
          paste("BBCH", bbch_stage)
        )
      )
  }

  # cols order
  colonnes_standard <- unname(lookup_table) # ex: plot_id, block_id, etc.
  colonnes_presentes <- names(df)
  colonnes_autres <- setdiff(colonnes_presentes, colonnes_standard)
  df <- df[, c(
    intersect(colonnes_standard, colonnes_presentes),
    colonnes_autres
  )]

  # add xp_trt_code from plot_id
  if ("plot_id" %in% names(df)) {
    df$xp_trt_code <- dplyr::case_when(
      grepl("^TNT", toupper(df$plot_id)) ~ "TNT",
      stringr::str_detect(df$plot_id, "[0-9]+") ~
        stringr::str_extract(df$plot_id, "[0-9]+"),
      TRUE ~ NA_character_
    )
  }

  return(df)
}

#' Pivot wider a resume dataframe
#'
#' @param df_resume a dataframe returned by resume_data function
#'
#' @returns a dataframe in wider format
#' @export
#' @importFrom tidyr pivot_wider
#'
resume_pivot_wider <- function(df_resume) {
  # local binding
  calculation <- value <- NULL

  if (!is.data.frame(df_resume)) {
    stop("Input must be a data frame")
  }

  if (
    "calculation" %in% colnames(df_resume) && "value" %in% colnames(df_resume)
  ) {
    df_resume %>%
      tidyr::pivot_wider(
        names_from = calculation,
        values_from = value
      ) -> df_resume
  } else {
    stop("Your dataframe MUST include cols 'calculation' and 'value'")
  }
  return(df_resume)
}


#' Prepare data
#'
#' @description
#' This function prepares a dataframe for analysis. It summarizes the data in a dataframe according to user-defined groups and functions (mean, frequency, intensity, etc.).
#' For instance, it allows you to calculate the intensity and frequency of disease attack per plot.
#' If the user wants to use the raw data, a "raw" parameter allows you to keep all the data from the initial dataframe.
#' The data is enriched by default with descriptions of the plots and treatments.
#'
#' @details
#' For efficacy calculation, tnt_mode could be one among :
#' - "all" : efficacy is calculated with the mean value of all TNT
#' - "block" : efficacy is calculated with the mean value of TNT per block
#' - "nearest" : efficacy is calculated with the nearest TNT to the plot
#' - "user" : efficacy is calculated with an association table between TNT and plot provided by the user (to import in (data_user)$plot_tnt_association$user_association)
#'
#' @param self an instance of the `UserData` R6 class containing observation data.
#' @param df the name of observation dataframe to prepare
#' @param var_cols character, colname(s) of the variable to use in data
#' @param group_cols colnames for grouping
#' @param funs vector of statistics to be applied, by default c("intensity","incidence")
#' @param filters a list of filters to apply. for instance : list(xp_trt_name = c("TNT","MOD1"), block_code = "A") will keep only block_code = A and xp_trt_name = TNT or MOD1.
#' @param code_tnt for efficacy only : a string to identify in TNT in the row of the dataframe by default "TNT"
#' @param raw boolean, if TRUE the dataframe returned contains all values without summarize, that means funs is ignored
#' @param tnt_mode (for efficacy calculation only) : mode of association of plot and tnt. a character value among "block","nearest","all","user". by default set to "all".
#' @param add_plot_desc boolean. if TRUE, the dataframe is merged with plot description found in the sheet "placette"
#' @param flex boolean. if TRUE, 10A A1O or 10a are considered as the same plot_id. if NULL, flex is automatically adjusted when a difference in plot_id is detected
#' @param add_trt_desc  boolean. if TRUE, the dataframe is merged with experimental treatment description found in the sheet "modalite"
#' @param prep_name name of the prepared dataset
#' @param prep_desc short description of the prepared dataset
#'
#' @returns a dataframe with group_cols and including a 'calculation' column which specifies the name of the functions applied to the variable and a 'value' column which gives the calculated values.
#' @export
#' @importFrom dplyr ensym syms filter group_by summarise n rename bind_rows select

prepare_data <- function(
  self,
  df,
  var_cols = NULL,
  group_cols = "plot_id",
  funs = list(intensite = intensity, frequence = incidence),
  filters = NULL,
  code_tnt = "TNT",
  raw = FALSE,
  tnt_mode = "all",
  add_plot_desc = TRUE,
  flex = NULL,
  add_trt_desc = TRUE,
  prep_name = NULL,
  prep_desc = NULL
) {
  # local binding
  calculation <- plot_id <- . <- value <- nb <- clean_id <- NULL

  # data
  data <- NULL
  if (df %in% names(self$obs_data)) {
    message("dataframe ", df, " found in observations data")
    data <- self$obs_data[[df]]
  }
  if (df %in% names(self$prepared_data)) {
    message("dataframe ", df, " found in prepared data")
    data <- self$prepared_data[[df]]
  }
  if (is.null(data)) {
    message(
      "dataframe ",
      df,
      " not found in either obs_data or prepared_data. Function aborted"
    )
    return(NULL)
  }

  # if var_cols not provided, try to gess the variable to be prepared
  if(is.null(var_cols)) {
    if("value" %in% names(data)) {
      var_cols <- "value"  ## assuming data is a prepared_dataframe
    } else {
      candidates_vars <- find_vars(data) ## find vars that have a match in start dictionary
      if(length(candidates_vars)>0) {
        message("Variable choosen",candidates_vars[1]) # choose the first one
        var_cols <- candidates_vars[1]
    }
    }
  }

  var_col_diff <- setdiff(var_cols, names(data)[sapply(data, is.numeric)])
  if (length(var_col_diff) > 0) {
    message("var cols: ", paste(var_col_diff, collapse = ","), " not found")
    message(paste(var_col_diff, collapse = ","), " removed from var cols")
    var_cols <- var_cols[!var_cols %in% var_col_diff]
  }

  if (is.null(var_cols)) {
    message(
      "var_cols can not be empty. please give a vector of numeric colnames among the following : ",
      paste(names(data)[sapply(data, is.numeric)], collapse = ",")
    )
    return(NULL)
  }

  ## check if grouping cols are in data
  grp_col_diff <- setdiff(group_cols, colnames(data))
  if (length(grp_col_diff) > 0) {
    message("group cols: ", paste(grp_col_diff, collapse = ","), " not found")
    message(paste(grp_col_diff, collapse = ","), " removed from grouping cols")
    group_cols <- group_cols[!group_cols %in% grp_col_diff]
  }

  if (length(group_cols) == 0) {
    message("no group cols. function aborted")
    return(NULL)
  }

  ## check correspondance between plot_id to adjust flex if not provided
  if(is.null(flex)) {
    if(check_plotid_diff(self$metadata$plot_desc, data)) {
      message("flex automatically set to TRUE to try to find equivalence in plot_id such as 10A = A10")
      flex <- TRUE} else {
        flex <- FALSE}
  }

  # check filters
  if (!is.null(filters)) {
    data2flt <- data
    flt_col_diff <- setdiff(names(filters), colnames(data2flt))
    if (length(flt_col_diff) > 0) {
      all_data <- merge_data_plotdesc(self, data2flt, flex = flex) # to try to find filters in metadata
      all_data <- merge_data_xpdesc(self, all_data) # to try to find filters in metadata
      flt_col_diff2 <- setdiff(names(filters), colnames(all_data))
      if (length(setdiff(flt_col_diff, flt_col_diff2)) > 0) {
        message("metadata added for filtering")
        data2flt <- all_data
      }
      if (length(flt_col_diff2) > 0) {
        message(
          "filters cols: ",
          paste(flt_col_diff2, collapse = ","),
          "not found"
        )
        message(paste(flt_col_diff2, collapse = ","), "removed from filters")
        filters <- filters[!names(filters) %in% flt_col_diff2]
      }
    }
    # filtering
    if (length(filters) > 0) {
      for (col in names(filters)) {
        data2flt %>% dplyr::filter(.data[[col]] %in% filters[[col]]) -> data2flt
      }
      if (nrow(data2flt) == 0) {
        message("no more data after filtering. function aborted")
        return(NULL)
      }
      message(paste(
        nrow(data) - nrow(data2flt),
        " rows removed from filtering"
      ))
      message(paste(nrow(data2flt), " rows after filtering"))
      data <- data2flt
    }
  }

  # convert column argument to symbol
  vars <- dplyr::syms(var_cols)
  group_syms <- dplyr::syms(group_cols)
  group_tnt <- NULL

  # # if calculation is in colnames (means that this is already a df from a previous iteration of resume_data)
  if ("calculation" %in% colnames(data)) {
    group_tnt <- dplyr::syms("calculation") #
    group_syms <- dplyr::syms(c(group_cols, "calculation"))
  }

  if (
    tnt_mode == "block" & is.null(self$plot_tnt_association$block_association)
  ) {
    message("generation of block_association table")
    block_tnt(self)
  }

  if (
    tnt_mode == "nearest" &
      is.null(self$plot_tnt_association$nearest_association)
  ) {
    message("generation of nearest_association table")
    nearest_tnt(self)
  }

  df_tnt <- switch(
    tnt_mode,
    "all" = NULL,
    "block" = self$plot_tnt_association$block_association,
    "nearest" = self$plot_tnt_association$nearest_association,
    "user" = self$plot_tnt_association$user_association
  )

  if (is.null(df_tnt) & tnt_mode != "all") {
    message(
      tnt_mode,
      " association dataframe between plot and TNT not in user_data, please execute block_tnt or nearest_tnt, or provide an user association table"
    )
    message(
      "in case of efficacy calculation, the mean of all ",
      code_tnt,
      " will be used"
    )
  }

  # check of df_tnt if not null (for efficacy calculation)
  if (!is.null(df_tnt)) {
    # check if df_tnt is a dataframe
    stopifnot(is.data.frame(df_tnt))
    # check if tnt_id is a col in  'df_tnt'
    if (!("tnt_id" %in% colnames(df_tnt))) {
      stop("'tnt_id' must be a col of 'df_tnt'.")
    }
    group_tnt <- c(group_tnt, dplyr::syms("plot_id")) ## to calculate var for each plot_id identified as TNT
  }

  ## for each var in var_cols
  all_data_resume <- data.frame()
  for (v in 1:length(vars)) {
    var <- vars[[v]]
    # raw
    if (raw) {
      # data$calculation <- as.character(var)
      # data$nb = 1
      var_name <- rlang::as_string(var)
      data %>%
        dplyr::rename(value = !!var) %>%
        dplyr::mutate(
          calculation = var_name,
          nb = as.integer(!is.na(value))
        ) %>%
        dplyr::select(c(!!!group_syms, calculation, value, nb)) -> data_resume
      if (nrow(data_resume) != nrow(data)) {
        stop("Error in raw extraction")
      }
      message("Argument raw set to TRUE : funs argument is ignored")
      all_data_resume <- dplyr::bind_rows(all_data_resume, data_resume)
      next
    }

    data_resume <- data.frame()
    for (i in 1:length(funs)) {
      if (identical(funs[[i]], startbox::efficacy)) {
        # filter data for tnt rows
        data %>%
          dplyr::filter(apply(
            .,
            1,
            function(row) any(grepl(code_tnt, row))
          )) -> data_tnt_filtered
        if (nrow(data_tnt_filtered) == 0) {
          all_data <- merge_data_plotdesc(self, data, flex = flex) # to try to find code_tnt in metadata
          all_data <- merge_data_xpdesc(self, all_data) # to try to find code_tnt in metadata
          if(!is.null(all_data)) {
          all_data %>%
            dplyr::filter(apply(
              .,
              1,
              function(row) any(grepl(code_tnt, row))
            )) -> data_tnt_filtered
          } else {
            message("sorry, code_tnt not found")
            return(NULL)
            }
        }

        ## calcul of tnt mean by group_tnt
        data_tnt_filtered %>%
          dplyr::group_by(!!!group_tnt) %>%
          dplyr::summarise(
            mean_tnt = mean({{ var }}, na.rm = T),
            nb_tnt = dplyr::n(),
            .groups = "drop"
          ) -> mean_tnt

        if (is.na(mean_tnt[1, 1]) | nrow(mean_tnt) == 0) {
          warning(
            "⚠️ No TNT value(s) found for efficacy calculation. Skipping this iteration."
          )
          next
        } else {
          print(paste(tnt_mode, code_tnt, "used for calculation of efficacy"))
        }

        if ("plot_id" %in% colnames(mean_tnt)) {
          mean_tnt %>%
            dplyr::rename(tnt_id = plot_id) -> mean_tnt
        }

        ## join with data
        if (!is.null(df_tnt)) {
          merge(df_tnt, mean_tnt) -> mean_tnt
          ## in case of more than one tnt by block_code or plot_id
          cols_to_group <- setdiff(
            names(mean_tnt),
            c("tnt_id", "mean_tnt", "nb_tnt")
          )
          mean_tnt %>%
            dplyr::group_by(across(all_of(cols_to_group))) %>%
            dplyr::summarise(
              mean_tnt = mean(mean_tnt, na.rm = T),
              .groups = "drop"
            ) -> mean_tnt
        }

        if (!is.null(group_tnt)) {
          if (flex) {
            mean_tnt$clean_id <- harmonize_plot_id_format(mean_tnt$plot_id) # harmonize plot_id
            tmp_data <- data
            tmp_data$clean_id <- harmonize_plot_id_format(tmp_data$plot_id) # harmonize plot_id
            tmp_data <- tmp_data[,
              !names(tmp_data) %in% c("plot_id", "block_code")
            ] ## removing plot_id
            # join data and plot description
            data <- dplyr::left_join(
              tmp_data,
              mean_tnt,
              by = c("clean_id", "calculation")
            ) %>%
              dplyr::select(-clean_id)
          } else {
            data <- merge(data, mean_tnt)
          }
        } else {
          data$mean_tnt = as.numeric(mean_tnt$mean_tnt)
        }

        resume <- data %>%
          dplyr::group_by(!!!group_syms) %>%
          dplyr::reframe(
            mean_tnt = mean(mean_tnt),
            value = efficacy({{ var }}, value_tnt = mean_tnt),
            nb =  sum(!is.na({{ var }})) #dplyr::n()
          )
      } else {
        # end of efficacy case
        resume <- data %>%
          dplyr::group_by(!!!group_syms) %>%
          dplyr::summarise(
            value = funs[[i]]({{ var }}),
            nb =  sum(!is.na({{ var }})), #dplyr::n(),
            .groups = "drop"
          )
      }
      ## to add calculation
      var_name <- rlang::as_string(var)
      # default_name <- ifelse(
      #   var != "value",
      #   paste(names(funs)[i], var),
      #   names(funs)[i]
      # )
      default_name <- if (var_name != "value") {
        paste(names(funs)[i], var_name)
      } else {
        names(funs)[i]
      }
      resume %>%
        dplyr::mutate(
          calculation = if ("calculation" %in% colnames(.))
            paste(default_name, calculation) else default_name
        ) -> resume
      ## to add to resume data
      data_resume <- dplyr::bind_rows(data_resume, resume)
    }
    all_data_resume <- dplyr::bind_rows(all_data_resume, data_resume)
  }

  ## to add plot desc
  if (add_plot_desc) {
    all_data_resume <- merge_data_plotdesc(self, all_data_resume, flex = flex)
  }

  if (add_trt_desc) {
    all_data_resume <- merge_data_xpdesc(self, all_data_resume)
  }

  if (is.null(prep_name)) {
    n_prp <- sum(grepl(df, names(self$prepared_data)))
    prep_name <- paste0("prp", n_prp + 1, "_", df)
  }

  if (is.null(prep_desc)) {
    prep_desc <- paste(
      paste(names(funs), collapse = " and "),
      "of (",
      paste(var_cols, collapse = " and "),
      ") grouped by (",
      paste(group_cols, collapse = " and "),
      ") calculated on the dataset",
      df
    )
    if (!is.null(filters)) {
      for (i in 1:length(filters)) {
        prep_desc <- paste(
          prep_desc,
          ifelse(i > 1, "and", ""),
          "filtered by",
          names(filters)[i],
          "=",
          paste(filters[[i]], collapse = " or ")
        )
      }
    }
    if (add_plot_desc) {
      prep_desc <- paste(prep_desc, "merged with plot description")
    }
    if (add_trt_desc) {
      prep_desc <- paste(prep_desc, "merged with treatment description")
    }
  }

  attr(all_data_resume, "description") <- prep_desc

  flag_resume <- all(c("value", "calculation") %in% colnames(all_data_resume))
  # pivoting resume_data
  if (!flag_resume) {
    stop(
      "Error : calculation and value cols are not included in the dataframe to return"
    )
  }

  ## to harmonize col types
  all_data_resume <- startbox::harmonize_column_types(all_data_resume)

  self$prepared_data[[prep_name]] <- all_data_resume
  message(paste0(
    "Prepared data saved in [data_user object]$prepared_data$",
    prep_name
  ))
}

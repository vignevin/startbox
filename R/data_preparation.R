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
    if (!file.exists(dictionary_path)) {
      stop("no dictionary found")
    }
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
  if (nrow(types_df) == 0) {
    stop("empty dictionary or missing data")
  }
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
    message("[merge_data_plotdesc] plot_desc missing.")
    return(NULL)
  }

  ## check if plot_id if in both dataframes
  if (
    !"plot_id" %in% intersect(colnames(self$metadata$plot_desc), colnames(df))
  ) {
    message("[merge_data_plotdesc] Col plot_id not in both dataframes")
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
      "[merge_data_plotdesc] WARNING : at least one plot_id in the dataset was not found in the sheet placette (plot_desc in metadata)"
    )
  }

  if (flex) {
    message("[merge_data_plotdesc] flex merging for plot_id. 10A = A10 = a10")
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
    message("[merge_data_plotdesc] no common plot_id !")
    return(NULL)
  }

  if (nrow(df_plot_desc) < nrow(df)) {
    message("[merge_data_plotdesc] WARNING : we lost data ! (unknown error)")
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
    message("[merge_data_xpdesc] moda_desc missing.")
    return(NULL)
  }

  ## check if plot_id if in both dataframes
  if (
    !"xp_trt_code" %in%
      intersect(colnames(self$metadata$moda_desc), colnames(df))
  ) {
    message("[merge_data_xpdesc] Col xp_trt_code not in both dataframes")
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
      "[merge_data_xpdesc] WARNING : at least one xp_trt_code in the dataset was not found in the sheet modalite (moda_desc in metadata)"
    )
  }

  # join data and plot description
  df_moda_desc <- dplyr::left_join(
    df,
    self$metadata$moda_desc,
    by = "xp_trt_code"
  )
  if (nrow(df_moda_desc) == 0) {
    message("[merge_data_xpdesc] no common xp_trt_code !")
    return(NULL)
  }

  if (nrow(df_moda_desc) < nrow(df)) {
    message("[merge_data_xpdesc] WARNING : we lost data ! (unknown error)")
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
#' @importFrom dplyr ensym syms filter group_by summarise n rename bind_rows select if_any everything

prepare_data <- function(
  self,
  df,
  var_cols = NULL,
  group_cols = "plot_id",
  funs = list(intensite = intensity, frequence = incidence),
  filters = NULL,
  code_tnt = "TNT",
  raw = FALSE,
  tnt_mode = c("block", "all", "nearest", "user"),
  add_plot_desc = TRUE,
  flex = NULL,
  add_trt_desc = TRUE,
  prep_name = NULL,
  prep_desc = NULL
) {
  # local binding
  calculation <- plot_id <- . <- value <- nb <- clean_id <- NULL

  # param check
  tnt_mode <- match.arg(tnt_mode)

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

  # 2. if var_cols not provided, try to gess the variable to be prepared
  var_cols <- resolve_var_cols(data, var_cols)
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
  if (is.null(flex)) {
    
    has_plot_desc <- !is.null(self$metadata$plot_desc) && 
      nrow(self$metadata$plot_desc) > 0 && 
      "plot_id" %in% colnames(self$metadata$plot_desc)
    
    # Si on a une description, on lance la comparaison
    check_diff <- if (has_plot_desc) {
      check_plotid_diff(self$metadata$plot_desc, data)
    } else {
      FALSE 
    }
    if (isTRUE(check_diff)) { 
      message(
        "flex automatically set to TRUE to try to find equivalence in plot_id such as 10A = A10"
      )
      flex <- TRUE
    } else {
      flex <- FALSE
    }
  }

  # 5. check and apply filters
  data <- apply_filters(self, data, filters, flex)
  if (is.null(data)) return(NULL)

  # convert column argument to symbol
  vars <- dplyr::syms(var_cols)
  group_syms <- dplyr::syms(group_cols)
  group_tnt <- NULL

  # # if calculation is in colnames (means that this is already a df from a previous iteration of resume_data)
  if ("calculation" %in% colnames(data)) {
    group_tnt <- dplyr::syms("calculation") #
    group_syms <- dplyr::syms(c(group_cols, "calculation"))
  }

  # 6.get df_tnt
  df_tnt <- prepare_tnt_association(self, tnt_mode, code_tnt)
  group_tnt <- c(group_tnt, dplyr::syms("plot_id")) ## to calculate var for each plot_id identified as TNT

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
        ## START for EFFICACY
        # filter data for tnt rows
        data %>%
          dplyr::filter(
            plot_id %in% get_tnt_ids_from_association(df_tnt = df_tnt)
          ) -> data_tnt_filtered
        if (nrow(data_tnt_filtered) == 0) {
          message("sorry, no tnt found in data for efficacy calculation")
          return(NULL)
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
            nb = sum(!is.na({{ var }})) #dplyr::n()
          ) %>%
          dplyr::filter(
            !plot_id %in% get_tnt_ids_from_association(df_tnt = df_tnt)
          )
      } else {
        # end of efficacy case
        resume <- data %>%
          dplyr::group_by(!!!group_syms) %>%
          dplyr::summarise(
            value = funs[[i]]({{ var }}),
            nb = sum(!is.na({{ var }})), #dplyr::n(),
            .groups = "drop"
          )
      }
      ## to add calculation
      var_name <- rlang::as_string(var)
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

  ## add description
  if (is.null(prep_desc)) {
    prep_desc <- generate_description(
      df,
      var_cols,
      group_cols,
      funs,
      filters,
      add_plot_desc,
      add_trt_desc
    )
  }
  if (is.null(all_data_resume)) {
    message(
      "error no prepared data to return. Please consider to set add_plot_desc and/or add_trt_desc to TRUE"
    )
    return(NULL)
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


#' Resolve and validate variable columns
#'
#' @description
#' Determines which columns to use as variables, guesses if not provided,
#' and ensures they are numeric
#'
#' @param data Dataframe to process
#' @param var_cols Character vector of column names, or NULL to auto-detect
#'
#' @returns Character vector of validated numeric column names, or NULL if none found
#'
#' @keywords internal
#' @noRd
resolve_var_cols <- function(data, var_cols) {
  if (is.null(var_cols)) {
    var_cols <- guess_var_cols(data)
  }

  if (is.null(var_cols)) {
    message("var_cols cannot be empty")
    return(NULL)
  }

  # Vérifier que les colonnes existent
  var_cols <- validate_cols_exist(data, var_cols, "numeric")

  if (length(var_cols) == 0) {
    message("No valid numeric columns found")
    return(NULL)
  }

  return(var_cols)
}

#' Guess variable columns from data
#'
#' @description
#' Attempts to identify appropriate variable columns by checking for "value"
#' column or using find_vars() to match against data dictionary
#'
#' @param data Dataframe to analyze
#'
#' @returns Character. Name of guessed variable column, or NULL if none found
#'
#' @keywords internal
#' @noRd
guess_var_cols <- function(data) {
  if ("value" %in% names(data)) {
    return("value")
  }

  candidates <- find_vars(data)
  if (length(candidates) > 0) {
    message("Variable chosen: ", candidates[1])
    return(candidates[1])
  }

  return(NULL)
}


#' Validate that columns exist in data
#'
#' @description
#' Checks that specified columns exist in the dataframe and optionally
#' filters by column type
#'
#' @param data Dataframe to check
#' @param cols Character vector of column names to validate
#' @param type Character. Optional type filter ("numeric" or NULL for any type)
#'
#' @returns Character vector of valid column names
#'
#' @keywords internal
#' @noRd
validate_cols_exist <- function(data, cols, type = NULL) {
  valid_cols <- if (is.null(type)) {
    intersect(cols, colnames(data))
  } else if (type == "numeric") {
    intersect(cols, names(data)[sapply(data, is.numeric)])
  } else {
    intersect(cols, colnames(data))
  }

  removed <- setdiff(cols, valid_cols)
  if (length(removed) > 0) {
    message(
      "Columns removed (not found or type non ",
      type,
      ") :",
      paste(removed, collapse = ", ")
    )
  }

  return(valid_cols)
}

# Filtering functions

#' Apply filters to data
#'
#' @description
#' Filters data based on user-specified conditions. Enriches data with metadata
#' if needed to access filter columns
#'
#' @param self UserData R6 object
#' @param data Dataframe to filter
#' @param filters Named list of filter conditions. Names are column names,
#'   values are vectors of acceptable values
#' @param flex Logical. Whether to use flexible plot ID matching
#'
#' @returns Filtered dataframe, or NULL if no rows remain after filtering
#'
#' @keywords internal
#' @noRd
apply_filters <- function(self, data, filters, flex) {
  if (is.null(filters) || length(filters) == 0) {
    return(data)
  }

  # Enrichir avec métadonnées si nécessaire pour le filtrage
  data_to_filter <- ensure_filter_cols(self, data, filters, flex)

  # Valider les colonnes de filtre
  filters <- validate_filter_cols(data_to_filter, filters)
  if (length(filters) == 0) return(data)

  # Appliquer les filtres
  filtered_data <- apply_filter_conditions(data_to_filter, filters)

  if (nrow(filtered_data) == 0) {
    message("No data after filtering. Function aborted")
    return(NULL)
  }

  message(nrow(data) - nrow(filtered_data), " rows removed by filtering")
  message(nrow(filtered_data), " rows remaining")

  return(filtered_data)
}

#' Ensure filter columns are available
#'
#' @description
#' Checks if filter columns exist in data. If not, attempts to add them
#' by merging with plot and treatment metadata
#'
#' @param self UserData R6 object
#' @param data Dataframe to enrich
#' @param filters Named list of filters
#' @param flex Logical. Whether to use flexible plot ID matching
#'
#' @returns Dataframe, potentially enriched with metadata
#'
#' @keywords internal
#' @noRd
ensure_filter_cols <- function(self, data, filters, flex) {
  # local binding
  . <- NULL

  missing_cols <- setdiff(names(filters), colnames(data))

  if (length(missing_cols) == 0) return(data)

  # try to add metadata
  enriched_data <- data %>%
    merge_data_plotdesc(self, ., flex = flex) %>%
    merge_data_xpdesc(self, .)

  still_missing <- setdiff(names(filters), colnames(enriched_data))

  if (length(still_missing) < length(missing_cols)) {
    message("Metadata added for filtering")
    return(enriched_data)
  }

  return(data)
}

#' Validate filter columns
#'
#' @description
#' Removes filters whose columns don't exist in the data
#'
#' @param data Dataframe to check against
#' @param filters Named list of filters
#'
#' @returns Named list of valid filters only
#'
#' @keywords internal
#' @noRd
validate_filter_cols <- function(data, filters) {
  valid_filters <- filters[names(filters) %in% colnames(data)]

  removed <- setdiff(names(filters), names(valid_filters))
  if (length(removed) > 0) {
    message("Filter columns removed: ", paste(removed, collapse = ", "))
  }

  return(valid_filters)
}

#' Apply filter conditions to data
#'
#' @description
#' Iterates through filters and applies each condition sequentially
#'
#' @param data Dataframe to filter
#' @param filters Named list of filters. Each filter keeps rows where
#'   the column value is in the specified vector
#'
#' @returns Filtered dataframe
#'
#' @keywords internal
#' @noRd
apply_filter_conditions <- function(data, filters) {
  for (col in names(filters)) {
    data <- data %>%
      dplyr::filter(.data[[col]] %in% filters[[col]])
  }
  return(data)
}

# TNT (untreated control) association functions

#' Prepare TNT association table
#'
#' @description
#' Retrieves or generates the appropriate plot-to-TNT association table
#' based on the specified mode (block, nearest, user, or all)
#'
#' @param self UserData R6 object
#' @param tnt_mode Character. One of "all", "block", "nearest", "user"
#' @param code_tnt Character. String identifying TNT plots
#'
#' @returns Dataframe with TNT associations, or NULL if tnt_mode is "all"
#'
#' @keywords internal
#' @noRd
prepare_tnt_association <- function(self, tnt_mode, code_tnt) {
  
  if (is.null(self$metadata$plot_desc) || nrow(self$metadata$plot_desc) == 0) {
    message("no metadata found : please consider to complete plot and treatment description in the Excel file before proceeding")
    return(NULL)
  }
  
  if (tnt_mode == "all" && is.null(self$plot_tnt_association$mean)) {
    message("Looking TNT plot in metadata")
    self$plot_tnt_association$mean <- data.frame(
      tnt_id = extract_tnt_from_metadata(self, code_tnt)
    )
  }

  # association genereation if needed
  if (
    tnt_mode == "block" && is.null(self$plot_tnt_association$block_association)
  ) {
    message("Generating block_association table")
    block_tnt(self)
  }

  if (
    tnt_mode == "nearest" &&
      is.null(self$plot_tnt_association$nearest_association)
  ) {
    message("Generating nearest_association table")
    nearest_tnt(self)
  }

  # get the association table
  df_tnt <- switch(
    tnt_mode,
    "all" = self$plot_tnt_association$mean,
    "block" = self$plot_tnt_association$block_association,
    "nearest" = self$plot_tnt_association$nearest_association,
    "user" = self$plot_tnt_association$user_association,
    NULL
  )

  # validate the association
  if (!is.null(df_tnt)) {
    stopifnot(is.data.frame(df_tnt))
    if (!("tnt_id" %in% colnames(df_tnt))) {
      stop("'tnt_id' must be a column of df_tnt")
    }
  }
  return(df_tnt)
}

#' Extract TNT plot IDs from metadata
#'
#' @description
#' Searches plot descriptions and treatment descriptions for TNT identifiers
#'
#' @param self UserData R6 object
#' @param code_tnt Character. TNT identifier string
#'
#' @returns Character vector of TNT plot_id values, or NULL if none found
#'
#' @keywords internal
#' @noRd
extract_tnt_from_metadata <- function(self, code_tnt) {
  tnt_ids <- c()

  ## extract and merge metata moda and plot
  mdata <- self$metadata$plot_desc
  if (!is.null(mdata)) {
    if (!is.null(self$metadata$moda_desc)) {
      mdata <- merge(mdata, self$metadata$moda_desc)
    }
    # Identify character/factor cols
    char_cols <- names(mdata)[sapply(mdata, function(x) {
      is.character(x) || is.factor(x)
    })]

    if (length(char_cols) > 0 && "plot_id" %in% names(mdata)) {
      # create a mask for rows that contains code_tnt
      tnt_pattern <- paste0("\\b", code_tnt, "\\b")

      contains_tnt <- apply(mdata[, char_cols, drop = FALSE], 1, function(row) {
        any(sapply(row, function(cell) {
          if (is.na(cell)) return(FALSE)
          grepl(tnt_pattern, as.character(cell), ignore.case = TRUE)
        }))
      })

      tnt_ids <- c(tnt_ids, mdata$plot_id[contains_tnt])
    }
    if (!is.null(tnt_ids) && length(tnt_ids) > 0) {
      message(sprintf(
        "Using metadata to identify TNT: found %d TNT plot(s)",
        length(tnt_ids)
      ))
      return(unique(tnt_ids))
    } else {
      message("No TNT plots found in metadata")
      return(NULL)
    }
  }
}

#' Get TNT plot IDs from association table
#'
#' @description
#' Retrieves the unique TNT plot identifiers from the appropriate association table
#' based on the TNT mode. This is the primary and most reliable method for
#' identifying which plots are TNT (untreated controls).
#'
#' @param self UserData R6 object containing plot_tnt_association tables
#' @param tnt_mode Character. One of "all", "block", "nearest", "user"
#' @param df_tnt Dataframe. TNT association table (optional, will be retrieved if NULL)
#'
#' @returns Character vector of unique TNT plot_id values, or NULL if:
#'   - association table doesn't exist
#'   - association table doesn't have tnt_id column
#'
#' @details
#' The function retrieves TNT IDs from:
#' - "block" mode: self$plot_tnt_association$block_association
#' - "nearest" mode: self$plot_tnt_association$nearest_association
#' - "user" mode: self$plot_tnt_association$user_association
#' - "all" mode: self$plot_tnt_association$mean
#'
#' @examples
#' \dontrun{
#' # Get TNT IDs for block mode
#' tnt_ids <- get_tnt_ids_from_association(user_data, tnt_mode = "block")
#'
#' # Get TNT IDs from provided association table
#' tnt_ids <- get_tnt_ids_from_association(
#'   user_data,
#'   tnt_mode = "block",
#'   df_tnt = my_custom_association
#' )
#' }
#'
#' @keywords internal
#' @noRd
get_tnt_ids_from_association <- function(
  self = NULL,
  tnt_mode = NULL,
  df_tnt = NULL
) {
  # if all is NULL
  if (is.null(self) & is.null(tnt_mode) & is.null(df_tnt)) {
    message("provide self and tnt_mode OR df_tnt")
    return(NULL)
  }

  # if df_tnt not provided, extract from self
  if (is.null(df_tnt)) {
    df_tnt <- switch(
      tnt_mode,
      "all" = self$plot_tnt_association$mean,
      "block" = self$plot_tnt_association$block_association,
      "nearest" = self$plot_tnt_association$nearest_association,
      "user" = self$plot_tnt_association$user_association,
      NULL
    )

    if (is.null(df_tnt)) {
      warning(sprintf(
        "No TNT association table found for mode '%s'. ",
        "Available associations: %s",
        tnt_mode,
        paste(names(self$plot_tnt_association), collapse = ", ")
      ))
      return(NULL)
    }
  }

  # validation df_tnt
  if (!is.data.frame(df_tnt)) {
    warning("df_tnt is not a dataframe")
    return(NULL)
  }

  if (!"tnt_id" %in% names(df_tnt)) {
    warning(sprintf(
      "Column 'tnt_id' not found in TNT association table. Available columns: %s",
      paste(names(df_tnt), collapse = ", ")
    ))
    return(NULL)
  }

  # unique tnt_id extraction
  tnt_ids <- unique(df_tnt$tnt_id)

  # remove NA
  tnt_ids <- tnt_ids[!is.na(tnt_ids)]

  if (length(tnt_ids) == 0) {
    warning("No valid tnt_id found in association table (all NA or empty)")
    return(NULL)
  }

  return(tnt_ids)
}

#' Generate automatic dataset description
#'
#' @description
#' Creates a human-readable description of what was done to prepare the data
#'
#' @param df Character. Source dataframe name
#' @param var_cols Character vector. Variables summarized
#' @param group_cols Character vector. Grouping variables
#' @param funs Named list. Functions applied
#' @param filters Named list or NULL. Filters applied
#' @param add_plot_desc Logical. Whether plot metadata was added
#' @param add_trt_desc Logical. Whether treatment metadata was added
#'
#' @returns Character. Description string
#'
#' @keywords internal
#' @noRd
generate_description <- function(
  df,
  var_cols,
  group_cols,
  funs,
  filters,
  add_plot_desc,
  add_trt_desc
) {
  desc <- paste(
    paste(names(funs), collapse = " and "),
    "of (",
    paste(var_cols, collapse = " and "),
    ")",
    "grouped by (",
    paste(group_cols, collapse = " and "),
    ")",
    "calculated on dataset",
    df
  )

  if (!is.null(filters)) {
    for (i in seq_along(filters)) {
      desc <- paste(
        desc,
        ifelse(i > 1, "and", ""),
        "filtered by",
        names(filters)[i],
        "=",
        paste(filters[[i]], collapse = " or ")
      )
    }
  }
  if (add_plot_desc) desc <- paste(desc, "merged with plot description")
  if (add_trt_desc) desc <- paste(desc, "merged with treatment description")
  return(desc)
}


#' Standardisation of POM csv file
#'
#' @description
#' Standardize a raw POM (EPICURE) csv file
#'
#' @param file Path to the data leading to a csv file exported from Epicure (IFV Information system)
#' @param skip_forecast Logical. If `TRUE` (default), the first 14 data rows are removed before processing (commonly forecast values).
#' @export
#'
#' @return A standardized data.frame with consistent column names and structure.
#'
#'
standardise_pom_csv = function(file, skip_forecast = TRUE) {
  # check input file
  if (!check_pom_csv(file)) {
    message("[import_meteo] ⛔ Import interrupted: essential columns missing.")
    return()
  }

  df <- readr::read_delim(
    file,
    delim = ";",
    locale = readr::locale(decimal_mark = "."),
    na = c("", "NA", "NaN"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  # Remove the first 14 lines as they are forecast data.
  if (skip_forecast) {
    if (nrow(df) > 14) {
      df <- df[-seq_len(14), , drop = FALSE]
      message(
        "[standardise_pom_csv] ℹ️ 14 firts lines removed (weather forecast)"
      )
    } else {
      warning(
        "[standardise_pom_csv] ⚠️ Less than 15 lines in the file, nothing deleted."
      )
    }
  }

  to_num <- function(x) {
    if (is.numeric(x)) return(x)
    as.numeric(as.character(x))
  }

  parse_date_base <- function(x) {
    x <- as.character(x)
    is_dmy_slash <- grepl("^\\d{2}/\\d{2}/\\d{4}$", x)
    if (any(is_dmy_slash)) {
      out <- as.Date(x, format = "%d/%m/%Y")
    } else {
      out <- rep(NA, length(x))
    }
    need <- is.na(out)
    if (any(need)) {
      tf <- c("%Y-%m-%d", "%d-%m-%Y", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y")
      out[need] <- as.Date(x[need], tryFormats = tf)
    }
    out
  }

  src_present <- names(df)

  # cols renaming
  if ("DATE" %in% src_present)
    df[["meteo_datetime"]] <- parse_date_base(df[["DATE"]])
  if ("TMIN" %in% src_present) df[["air_tmin_celsius"]] <- to_num(df[["TMIN"]])
  if ("TMAX" %in% src_present) df[["air_tmax_celsius"]] <- to_num(df[["TMAX"]])
  if ("TMOY" %in% src_present) df[["air_tmean_celsius"]] <- to_num(df[["TMOY"]])
  if ("PLUIE" %in% src_present) df[["rain_mm"]] <- to_num(df[["PLUIE"]])
  if ("ETP" %in% src_present) df[["etp_mm"]] <- to_num(df[["ETP"]])
  if ("UMOY" %in% src_present) df[["air_hmean_p"]] <- to_num(df[["UMOY"]])
  if ("UMIN" %in% src_present) df[["air_hmin_p"]] <- to_num(df[["UMIN"]])
  if ("UMAX" %in% src_present) df[["air_hmax_p"]] <- to_num(df[["UMAX"]])

  # input cols
  col_sources <- c(
    "DATE",
    "TMIN",
    "TMAX",
    "TMOY",
    "PLUIE",
    "ETP",
    "UMOY",
    "UMIN",
    "UMAX"
  )
  # output cols
  col_cibles <- c(
    "meteo_datetime",
    "air_tmin_celsius",
    "air_tmax_celsius",
    "air_tmean_celsius",
    "rain_mm",
    "etp_mm",
    "air_hmean_p",
    "air_hmin_p",
    "air_hmax_p"
  )

  # remove input cols
  keep <- setdiff(names(df), intersect(names(df), col_sources))
  df <- df[, keep, drop = FALSE]

  # ordering
  col_existantes <- names(df)
  std_cols <- intersect(col_cibles, col_existantes)
  extra_cols <- setdiff(col_existantes, std_cols)
  df <- df[, c(std_cols, extra_cols), drop = FALSE]

  message("[standardise_pom_csv] ✅ ", nrow(df), " standardised rows")
  return(df)
}

## update 2

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
    tnt_mode = c("block","all","nearest","user"),
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
    if (check_plotid_diff(self$metadata$plot_desc, data)) {
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
      if (identical(funs[[i]], startbox::efficacy)) {   ## START for EFFICACY
        # filter data for tnt rows
        data %>%
          dplyr::filter(plot_id %in% get_tnt_ids_from_association(df_tnt = df_tnt)
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
          dplyr::filter(!plot_id %in% get_tnt_ids_from_association(df_tnt = df_tnt))
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
      df, var_cols, group_cols, funs, filters, add_plot_desc, add_trt_desc
    )
  }
  if(is.null(all_data_resume)) {
    message("error no prepared data to return. Please consider to set add_plot_desc and/or add_trt_desc to TRUE")
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
    message("Columns removed (not found or type non ",type,") :", paste(removed, collapse = ", "))
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
ensure_filter_cols <- function(self, data, filters, flex) {
  missing_cols <- setdiff(names(filters), colnames(data))

  if (length(missing_cols) == 0) return(data)

  # Essayer d'ajouter les métadonnées
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
prepare_tnt_association <- function(self, tnt_mode, code_tnt) {
  if (tnt_mode == "all" && is.null(self$plot_tnt_association$mean)) {
    message("Looking TNT plot in metadata")
    self$plot_tnt_association$mean <- data.frame(tnt_id = extract_tnt_from_metadata(self,code_tnt))
  }

  # Générer les associations si nécessaire
  if (tnt_mode == "block" && is.null(self$plot_tnt_association$block_association)) {
    message("Generating block_association table")
    block_tnt(self)
  }

  if (tnt_mode == "nearest" && is.null(self$plot_tnt_association$nearest_association)) {
    message("Generating nearest_association table")
    nearest_tnt(self)
  }

  # Récupérer l'association appropriée
  df_tnt <- switch(
    tnt_mode,
    "all" = self$plot_tnt_association$mean,
    "block" = self$plot_tnt_association$block_association,
    "nearest" = self$plot_tnt_association$nearest_association,
    "user" = self$plot_tnt_association$user_association,
    NULL
  )

  # Valider l'association
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
extract_tnt_from_metadata <- function(self, code_tnt) {
  tnt_ids <- c()

  ## extract and merge metata moda and plot
  mdata <- self$metadata$plot_desc
  if(!is.null(mdata)) {
    if(!is.null(self$metadata$moda_desc)) {
      mdata <- merge(mdata, self$metadata$moda_desc)
    }
    # Identifier les colonnes character/factor
    char_cols <- names(mdata)[sapply(mdata, function(x) {
      is.character(x) || is.factor(x)
    })]

    if (length(char_cols) > 0 && "plot_id" %in% names(mdata)) {
      # Créer un masque pour les lignes contenant code_tnt
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
      message(sprintf("Using metadata to identify TNT: found %d TNT plot(s)",
                      length(tnt_ids)))
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
#' @export
get_tnt_ids_from_association <- function(self =NULL, tnt_mode = NULL, df_tnt = NULL) {
  # if all is NULL
  if(is.null(self)&is.null(tnt_mode)&is.null(df_tnt)) {
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
generate_description <- function(df, var_cols, group_cols, funs, filters,
                                 add_plot_desc, add_trt_desc) {
  desc <- paste(
    paste(names(funs), collapse = " and "),
    "of (", paste(var_cols, collapse = " and "), ")",
    "grouped by (", paste(group_cols, collapse = " and "), ")",
    "calculated on dataset", df
  )

  if (!is.null(filters)) {
    for (i in seq_along(filters)) {
      desc <- paste(
        desc,
        ifelse(i > 1, "and", ""),
        "filtered by", names(filters)[i], "=",
        paste(filters[[i]], collapse = " or ")
      )
    }
  }
  if (add_plot_desc) desc <- paste(desc, "merged with plot description")
  if (add_trt_desc) desc <- paste(desc, "merged with treatment description")
  return(desc)
}

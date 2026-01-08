## functions for in and out data

#' @title Save Combined Data to a Versioned Excel File
#'
#' @description
#' Saves the provided combined observation data into the "data" sheet of a versioned Excel file.
#' The function creates a copy of the original Excel file with an incremented suffix (e.g., `_v1`, `_v2`, etc.)
#' to avoid overwriting existing files.
#'
#' @param combined_data A `data.frame` containing the combined observation data to write into the Excel file.
#' @param excel_data_trial_path Character. Full path to the original Excel file to use as a base for creating the versioned copy.
#'
#' @return A character string containing the full path of the newly saved versioned Excel file.
#'
#' @details
#' - The original Excel file is not modified.
#' - If a "data" sheet exists in the versioned file, it is replaced with the new data.
#' - The function returns the path of the versioned file, which includes `_v1`, `_v2`, etc., to ensure uniqueness.
#'
#' @export
save_data_to_excel <- function(combined_data, excel_data_trial_path) {
  # Extract the path, name, and extension
  dir <- dirname(excel_data_trial_path)
  base <- tools::file_path_sans_ext(basename(excel_data_trial_path))
  ext <- tools::file_ext(excel_data_trial_path)

  # generation of versioned name
  i <- 1
  repeat {
    new_file <- file.path(dir, paste0(base, "_v", i, ".", ext))
    if (!file.exists(new_file)) break
    i <- i + 1
  }

  # copy of the original file
  success <- file.copy(
    from = excel_data_trial_path,
    to = new_file,
    overwrite = FALSE
  )
  if (!success) stop("❌ Unable to create versioned copy of Excel file.")

  # load the workbook
  wb <- openxlsx2::wb_load(new_file)

  # remove old sheet "data" if exists
  if ("data" %in% wb$sheet_names) {
    wb$remove_worksheet("data")
  }

  # add new data to the sheet "data"
  wb$add_worksheet("data")
  wb$add_data_table(sheet = "data", x = combined_data)
  wb$set_active_sheet("data")

  # save the copy fil with new datas
  wb$save(new_file)

  message(paste0("✅ Versioned Excel file saved as: ", new_file))
  return(new_file)
}

#' @title Prepare Excel Trial File from Template
#'
#' @description
#' Prepares the Excel trial file by copying the blank template if `self$excel_data_trial` is missing or invalid.
#' This ensures a valid Excel structure is available for inserting observation and metadata sheets.
#'
#' @param self An instance of the `UserData` R6 class.
#' @param directory Character. Directory where the new file should be saved. Defaults to the current working directory if NULL.
#' @param filename Character. Name of the new Excel file (must end with `.xlsx`). If NULL, a default name based on the template is used.
#'
#' @return None. The method updates `self$excel_data_trial` with the path to the copied file.
#'
#' @details
#' - If `self$excel_data_trial` is already set and the file exists, nothing is done.
#' - Otherwise, the method copies `self$excel_model` to the specified location and updates the reference in the object.
#' - A message is printed to indicate success or failure.
#'
#' @export
prepare_excel_model <- function(self, directory = NULL, filename = NULL) {
  # if is null excel_data_trial
  if (is.null(self$excel_data_trial) || !file.exists(self$excel_data_trial)) {
    message("📁 Creating the trial Excel file from the blank template...")

    # set filename
    if (is.null(filename)) {
      filename <- paste0(
        tools::file_path_sans_ext(basename(self$excel_model)),
        "_copie.xlsx"
      )
    }

    # set filepath
    if (is.null(directory)) {
      directory <- getwd() #  folder in use
    }

    full_path <- file.path(directory, filename)

    # template copy
    success <- file.copy(
      from = self$excel_model,
      to = full_path,
      overwrite = TRUE
    )

    if (success) {
      self$excel_data_trial <- full_path
      message(paste("Trial Excel created at:", full_path))
    } else {
      stop("Failed to create the trial Excel file.")
    }
  }
}

#' Load data from an standardized Excel file into R environment
#'
#' @param trial_file path to data excel model
#' @param name a name of the experiment to be stored
#'
#' @returns load data into a new user_data object in the R environment
#' @export
#'
#' @examples
#' mydata <- load_excel_file()
load_excel_file <- function(trial_file = NULL, name = NULL) {
  user_data$new(trial_file = trial_file, name = name)
}


#' Check if the file provided is a xlsx standardised file
#'
#' @param trial_file a file path
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @examples
#' check_standard_file(trial_file = system.file("extdata","standard_exemple.xlsx",package="startbox"))
check_standard_file <- function(trial_file = NULL) {
  if (!file.exists(trial_file)) {
    message("No file found")
    return(FALSE)
  }
  pos <- regexpr("\\.([[:alnum:]]+)$", trial_file)
  ext <- ifelse(pos > -1L, substring(trial_file, pos + 1L), "")
  if (ext != "xlsx") {
    message("This is not an xlsx file")
    return(FALSE)
  }
  wb_trial <- openxlsx2::wb_load(trial_file)
  metadata <- openxlsx2::wb_get_properties(wb_trial)
  if (!grepl("STAR", metadata[1])) {
    message("Warning, this Excel file seems not to be a standard file")
    return(FALSE)
  }
  return(TRUE)
}

#' @title Load Metadata Sheets from Excel Trial File
#'
#' @description
#' Reads the sheets named `"placette"` and `"modalite"` from the Excel trial file stored in `self$excel_data_trial`,
#' and stores the non-empty content into the metadata list of the `UserData` object.
#'
#' @param self An instance of the `UserData` R6 class containing the path to the Excel trial file.
#'
#' @return None. Updates `self$metadata` by assigning:
#' - `"plot_desc"` from the "placette" sheet if present and non-empty.
#' - `"moda_desc"` from the "modalite" sheet if present and non-empty.
#'
#' @details
#' - Empty rows (fully NA or empty strings) are removed from each sheet before storage.
#' - Messages are printed to confirm loading or alert if sheets are missing or empty.
#'
#' @export
load_metadata_sheets <- function(self) {
  wb_trial <- openxlsx2::wb_load(self$excel_data_trial)

  # Read and store 'dico' sheet
  if ("dictionary" %in% wb_trial$sheet_names) {
    dictionary <- openxlsx2::wb_to_df(
      wb_trial,
      sheet = "dictionary",
      skip_empty_rows = TRUE
    )

    # removing cols without names (NA or "")
    dictionary <- dictionary[, dplyr::coalesce(colnames(dictionary), "") != ""]

    if (nrow(dictionary) > 0) {
      self$dictionary <- dictionary
      message("✅ Sheet 'dictionary loaded ")
    } else {
      message("⚠️ Sheet 'dictionary ' empty. Default dictionary used")
      load_default_dictionary(self)
    }
  } else {
    message("ℹ️ Sheet 'dictionary' not found.")
    load_default_dictionary(self)
  }

  # Read and store 'placette' sheet
  if ("placette" %in% wb_trial$sheet_names) {
    placette_data <- openxlsx2::wb_to_df(
      wb_trial,
      sheet = "placette",
      skip_empty_rows = TRUE
    )

    # removing cols without names (NA or "")
    placette_data <- placette_data[,
                                   dplyr::coalesce(colnames(placette_data), "") != ""
    ]

    if (nrow(placette_data) > 0) {
      self$add_metadata("plot_desc", placette_data)
      message("✅ Sheet 'placette' loaded into metadata$placette.")
    } else {
      warning("⚠️ Sheet 'placette' is empty.")
    }
  } else {
    warning("ℹ️ Sheet 'placette' not found.")
  }

  # Read and store 'modalite' sheet
  if ("modalite" %in% wb_trial$sheet_names) {
    modalite_data <- openxlsx2::wb_to_df(
      wb_trial,
      sheet = "modalite",
      skip_empty_rows = TRUE
    )

    # removing cols without names (NA or "")
    moda_colnames <- colnames(modalite_data)
    modalite_data <- modalite_data[, dplyr::coalesce(moda_colnames, "") != ""]

    if ("xp_trt_name" %in% moda_colnames) {
      if (!"xp_trt_code" %in% moda_colnames) {
        warning(
          "⚠️ Sheet 'modalite' contains no col xp_trt_code. Please check your data file !"
        )
      } else {
        ## to fill xp_trt_name if missing (needed for later)
        if (
          sum(
            is.na(modalite_data$xp_trt_name) | modalite_data$xp_trt_name == ""
          ) >
          0
        ) {
          message(
            "⚠️ some xp_trt_name are missing. xp_trt_code used instead. Please check your data file"
          )
        }
        modalite_data$xp_trt_name <- ifelse(
          is.na(modalite_data$xp_trt_name) | modalite_data$xp_trt_name == "",
          modalite_data$xp_trt_code, # if exists keep
          modalite_data$xp_trt_name # else use xp_trt_code as xp_trt_name
        )
      }
    }

    ## convert data types
    modalite_data <- startbox::harmonize_column_types(
      modalite_data,
      dictionary = self$dictionary
    )

    if (nrow(modalite_data) > 0) {
      self$add_metadata("moda_desc", modalite_data)
      message("✅ Sheet 'modalite' loaded into metadata$modalite.")
    } else {
      message("⚠️ Sheet 'modalite' is empty.")
    }
  } else {
    message("ℹ️ Sheet 'modalite' not found.")
  }

  if ("ppp" %in% wb_trial$sheet_names) {
    ppp_data <- openxlsx2::wb_to_df(
      wb_trial,
      sheet = "ppp",
      skip_empty_rows = TRUE
    )

    # removing cols without names (NA or "")
    ppp_colnames <- colnames(ppp_data)
    ppp_data <- ppp_data[, dplyr::coalesce(ppp_colnames, "") != ""]

    ## convert data types
    ppp_data <- startbox::harmonize_column_types(
      ppp_data,
      dictionary = self$dictionary
    )

    if (nrow(ppp_data) > 0) {
      self$add_metadata("ppp", ppp_data)
      message("✅ Sheet 'ppp' loaded into metadata$ppp.")
    } else {
      message("⚠️ Sheet 'ppp' is empty.")
    }
  } else {
    message("ℹ️ Sheet 'ppp' not found.")
  }
}



# Function to get and save the template Excel file
#' get_template_excel
#'
#' @param destination_path the file path where the template will be saved
#'
#' @returns none, the template.xlsx file is saved on user's disk
#' @export
#'
get_template_excel <- function(destination_path = NULL) {
  # Construct the path to the file in the package
  file_path <- system.file("extdata", "template.xlsx", package = "startbox")

  # Check if the file exists
  if (file.exists(file_path)) {
    if (!is.null(destination_path)) {
      # Copy the file to the destination path specified by the user
      file.copy(file_path, destination_path)
      # Return a success message
      message("The file has been successfully saved to the specified location.")
    } else {
      # Construct the path to the user's Downloads directory
      download_path <- file.path(path.expand("~"), "modele_standard.xlsx")

      # Copy the file to the Downloads directory
      file.copy(file_path, download_path)

      # Return a success message
      message(paste("The file has been successfully saved to", download_path))
    }
  } else {
    # Return an error message if the file does not exist
    stop(
      "The specified file does not exist in the inst/extdata directory of the package."
    )
  }
}

#' @title Import "data_*" sheets from an Excel file into a `user_data` object
#'
#' @description
#' This function loads all Excel sheets whose names start with "data_" from the Excel file stored in `self$excel_data_trial`.
#' Each sheet is converted to a data.frame and stored in the `obs_data` list of the R6 object.
#' If a sheet already exists in `obs_data`, it will be replaced and a trace will be logged.
#'
#' @param self A `user_data` R6 object containing the Excel file path and observation data.
#'
#' @return Updates `self$obs_data` with the newly loaded sheets. Returns `invisible(self$obs_data)`.
#'
#' @export
load_data_sheets <- function(self) {
  filepath <- self$excel_data_trial

  if (is.null(filepath) || !file.exists(filepath)) {
    stop("❌ No valid Excel file found in self$excel_data_trial.")
  }

  wb <- openxlsx2::wb_load(filepath)
  sheets <- wb$sheet_names

  data_sheets <- sheets[grepl("^data_", sheets)]
  if (length(data_sheets) == 0) {
    message("ℹ️ No sheets starting with 'data_' found in file.")
    return(invisible(NULL))
  }

  for (sheet in data_sheets) {
    df <- tryCatch(
      {
        openxlsx2::wb_to_df(wb, sheet = sheet, skip_empty_rows = TRUE)
      },
      error = function(e) {
        warning(paste("⚠️ Could not read sheet:", sheet))
        return(NULL)
      }
    )

    if (is.null(df)) next

    # cleaning sheet names
    safe_sheet <- gsub("[:\\\\/*?\\[\\]]", "_", sheet)

    # removing cols without names (NA or "")
    df_clean <- df[, dplyr::coalesce(colnames(df), "") != ""]

    if (nrow(df_clean) > 0) {
      ## convert data types
      df_clean <- startbox::harmonize_column_types(
        df_clean,
        dictionary = self$dictionary
      )
      self$obs_data[[safe_sheet]] <- df_clean
      # add message if sheet already exists
      if (safe_sheet %in% names(self$obs_data)) {
        message("🔁 Sheet already loaded: ", safe_sheet, " → replaced.")

        filename_base <- basename(filepath)
        self$log_trace(
          operation = "update_data",
          filename = paste0(filename_base, ":", safe_sheet),
          description = "Updated sheet"
        )
      } else {
        message(paste("✅ Sheet", safe_sheet, "added into  obs_data"))
      }
    } else {
      message("⚠️ Sheet ", safe_sheet, " is empty and not loaded")
    }
  }
}

#' @title Export observation sheets into a new Excel file version
#'
#' @description
#' Exports all observation data stored in the `obs_data` slot of a `user_data` object into a new Excel file.
#' Existing "data_*" sheets are replaced, and new ones are added. The resulting file is saved in the Downloads folder
#' with an automatically generated timestamp in the filename. The function also updates the internal Excel file path
#' and logs all export operations in the traceability system.
#'
#' @param self A `user_data` R6 object containing observation data and the active Excel file path.
#'
#' @return Invisibly returns the full path to the exported Excel file.
#'
#' @export
export_data_sheets <- function(self) {
  wb <- openxlsx2::wb_load(self$excel_data_trial)
  added_sheets <- vector()
  for (i in seq_along(self$obs_data)) {
    original_name <- names(self$obs_data)[i]
    df <- self$obs_data[[i]]

    sheetname <- if (grepl("^data_", original_name)) {
      original_name
    } else {
      paste0("data_", tools::file_path_sans_ext(basename(original_name)))
    }

    if (sheetname %in% wb$sheet_names) {
      # the sheet is already in the Excel file : no change
      next
      #openxlsx2::wb_remove_worksheet(wb, sheet = sheetname)
      #message("🔁 Sheet replaced: ", sheetname)
    } else {
      wb$add_worksheet(sheetname)
      wb$add_data_table(sheet = sheetname, x = df)
      message("✅ Sheet added: ", sheetname)
      added_sheets <- c(added_sheets, sheetname)
    }
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d_%Hh%M")

  base_path <- normalizePath(self$excel_data_trial)
  base_file <- basename(base_path)
  name_no_ext <- tools::file_path_sans_ext(base_file)
  ext <- tools::file_ext(base_file)
  output_dir <- file.path(Sys.getenv("USERPROFILE"), "Downloads")

  new_filename <- file.path(
    output_dir,
    paste0(name_no_ext, "_", timestamp, ".", ext)
  )

  wb$set_sheet_visibility(sheet = "uri_list", value = "veryHidden") ## hide sheet listes
  wb$set_sheet_visibility(sheet = "listes", value = "veryHidden") ## hide sheet listes

  wb$save(file = new_filename)
  message("✅ New Excel file saved at: ", new_filename)

  # update file path
  self$excel_data_trial <- new_filename

  # Ajout dans traceability
  filename_base <- basename(new_filename)
  if (length(added_sheets > 0)) {
    for (sheet in added_sheets) {
      self$log_trace(
        operation = "export",
        filename = paste0(filename_base, ":", sheet),
        description = "Sheet exported"
      )
    }
  }
  # update of log sheet
  write_log(self)

  invisible(new_filename)
}

#' @title Wrapper to load all relevant data for plotting
#'
#' @description
#' This wrapper function loads all observation sheets (sheets named "data_*") and associated metadata
#' (e.g., "placette" and "modalite" sheets) from the Excel file into a `user_data` R6 object.
#' It updates both `obs_data` and `metadata` accordingly.
#'
#' @param self A `user_data` R6 object containing the Excel file path.
#'
#' @return Invisibly returns nothing. Updates the object in place with observation and metadata.
#'
#' @export
wrapper_data <- function(self) {
  # Step 1: import the data_* sheets (update self$obs_data)
  load_data_sheets(self)

  #Step 2 : import the "placette" and "modalite" sheets (update self$metadata)
  load_metadata_sheets(self)
}

#' @title Write log to Excel file
#'
#' @description
#' This function writes the content of the `traceability` slot from a `user_data` object into a sheet named "log"
#' in the associated Excel file. If the sheet already exists, the new entries are appended to the existing ones.
#'
#' @param self A `user_data` R6 object with a `traceability` data.frame and an `excel_data_trial` path.
#'
#' @return No return value. The Excel file is updated in place.
#'
#' @importFrom dplyr bind_rows
#' @importFrom openxlsx2 wb_load wb_to_df wb_remove_worksheet wb_add_worksheet wb_add_data wb_save
#'
#' @export
write_log <- function(self) {
  filepath <- self$excel_data_trial
  if (is.null(filepath) || !file.exists(filepath)) {
    stop("❌ No valid Excel file found in self$excel_data_trial.")
  }

  wb <- openxlsx2::wb_load(filepath)

  if ("log" %in% wb$sheet_names) {
    message("📑 Sheet 'log' already exists → appending new entries.")

    old_log <- openxlsx2::wb_to_df(wb, sheet = "log")
    updated_log <- dplyr::bind_rows(old_log, self$traceability)

    wb$remove_worksheet(sheet = "log")
    wb$add_worksheet(sheet = "log")
    wb$add_data(sheet = "log", x = updated_log, withFilter = FALSE)
  } else {
    message("🆕 Creating new sheet 'log'.")
    wb$add_worksheet(sheet = "log")
    wb$add_data(sheet = "log", x = self$traceability, withFilter = FALSE)
  }
  wb$set_sheet_visibility(sheet = "uri_list", value = "veryHidden") ## hide sheet listes
  wb$set_sheet_visibility(sheet = "listes", value = "veryHidden") ## hide sheet listes
  wb$save(file = filepath)
  message("✅ Log written to 'log' sheet in ", basename(filepath))
}

#' @title load default dictionary
#'
#' @param self A `user_data` R6 object with a `traceability` data.frame and an `excel_data_trial` path.
#'
#' @returns no return value, user_data dictionary is updated
#' @export
#'
load_default_dictionary <- function(self) {
  dictionary_path = system.file(
    "extdata",
    "star_dictionary.csv",
    package = "startbox"
  )
  if (!file.exists(dictionary_path)) {
    stop("no default dictionay found")
  }
  if (!is.null(self$dictionary)) {
    message(
      "Warning : existing dictionary will be replace by default dictionary"
    )
  }

  dictionary <- utils::read.csv2(dictionary_path, stringsAsFactors = FALSE)
  # keep only lines where both name and Rclass are non-empty and non-NA.
  dictionary <- dictionary[
    !(is.na(dictionary$nom) |
      dictionary$nom == "" |
      is.na(dictionary$Rclass) |
      dictionary$Rclass == ""),
  ]
  self$dictionary <- dictionary
}


#' @title Load meteo data from Excel sheet
#'
#' @description
#' This function reads the `meteo` sheet from the Excel file associated with a `user_data` and stores it in the slot `self$meteo`.
#' If the sheet is not found, `self$meteo` is set to `NULL`.
#'
#' @param self A `user_data` R6 object containing the `trial_file` attribute,
#'
#' @return Invisibly returns the loaded data.frame stored in `self$meteo`.
#'
#' @importFrom openxlsx2 wb_load wb_to_df
#' @importFrom dplyr coalesce
#' @export
load_meteo <- function(self) {
  wb_trial <- openxlsx2::wb_load(self$excel_data_trial)

  if ("meteo" %in% wb_trial$sheet_names) {
    meteo <- openxlsx2::wb_to_df(
      wb_trial,
      sheet = "meteo",
      skip_empty_rows = TRUE
    )

    meteo <- meteo[, dplyr::coalesce(base::colnames(meteo), "") != ""]

    ## harmonize col types according to dictionary
    meteo <- harmonize_column_types(meteo, dictionary = self$dictionary)

    if (nrow(meteo) > 0) {
      self$meteo <- meteo
      message("✅ Sheet 'meteo' loaded into self$meteo")
    } else {
      message("⚠️ Sheet 'meteo' is empty.")
      self$meteo <- NULL
    }
  } else {
    message("ℹ️ Sheet 'meteo' not found.")
    self$meteo <- NULL
  }

  invisible(self$meteo)
}


#' Import meteo data from a EPICURE POM csv file
#'
#' @description
#' This function imports meteorological data from a CSV file exported from the EPICURE Information System.
#' The data are standardized before being added to `self$meteo`.
#'
#' If meteorological data already exist in `self$meteo`, the `update` parameter allows the user to:
#' - `"new"`: append only the data more recent than the last `meteo_datetime` in `self$meteo`.
#' - `"all"`: replace all existing data with the new data.
#'
#' The function also updates the internal log (traceability).
#'
#' @param filepath File path of the csv file to be imported. This file must be a csv file exported from Epicure (IFV Information system)
#' @param skip_forecast Logical. If `TRUE` (default), the first 14 data rows are removed before processing (commonly forecast values).
#' @param self An `user_data` R6 object in which to import the meteo
#' @param update  Character. Determines how to update existing meteorological data: `"new"` (default) or `"all"`.
#' @returns Updates `self$meteo` with the imported data, update also the log. Returns `invisible(self$obs_data)`.
#' @details
#' The function automatically detects new rows based on the `meteo_datetime` column.
#' If `update = "new"` but `self$meteo` is empty, all data are imported.
#' A message is displayed indicating the number of rows imported.
#' @export
#'
#' @examples
#' \dontrun{
#' # Import new meteorological data into an existing user_data object
#' import_pom_csv(self = my_user_data, filepath = "meteo_2025.csv", update = "new")
#'
#' # Replace all existing meteorological data
#' import_pom_csv(self = my_user_data, filepath = "meteo_2025.csv", update = "all")
#' }
#'
import_pom_csv <- function(
  self,
  filepath,
  skip_forecast = TRUE,
  update = c("new", "all")
) {
  update <- match.arg(update, several.ok = FALSE)
  if (!update %in% c("new", "all")) {
    stop("Invalid value for `update`")
  }
  ## number of rows added
  ## check, read and standardise pom file
  df_mto <- standardise_pom_csv(file = filepath, skip_forecast = skip_forecast)
  if (exists("df_mto", inherits = FALSE)) {
    nrows_df_mto <- nrow(df_mto)
    nrows_added <- 0
    ## check if meteo alread exists in self and if it contains at least a col `meteo_datetime`
    if (update == "new") {
      if (is.null(self$meteo)) {
        warning(
          "Warning : no previous meteo data found in self. All data are imported"
        )
        self$meteo <- df_mto
        nrows_added <- nrows_df_mto
      }
      if (!is.null(self$meteo)) {
        if (!("meteo_datetime" %in% names(self$meteo))) {
          message(
            "[import_pom_csv] Col 'meteo_datetime' missing in meteo. Check previous meteo data and consider using update = 'all'",
            call. = FALSE
          )
        } else {
          last_date <- max(to_posix(self$meteo$meteo_datetime))
          rows_to_add <- to_posix(df_mto$meteo_datetime) > last_date
          self$meteo <- dplyr::bind_rows(self$meteo, df_mto[rows_to_add, ])
          self$meteo <- self$meteo[
            order(to_posix(self$meteo$meteo_datetime), decreasing = TRUE),
          ]
          nrows_added <- sum(rows_to_add)
          if (nrows_added == 0) {
            message("No NEW rows to import")
          }
        }
      }
    }
    if (update == "all") {
      self$meteo <- df_mto
      nrows_added <- nrows_df_mto
    }
    message(nrows_added, " rows imported in meteo")
    ########### TO DO ###########################################
    # if nrows_added>0 add a new line to traceability
  }
}

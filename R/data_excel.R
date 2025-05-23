#' @description
#' Save combined data into the 'data' sheet of an Excel file.
#'
#' @param combined_data The combined observation data (data.frame).
#' @param excel_data_trial_path Path to the Excel file (.xlsx).
#'
#' @return None. Writes and saves into Excel.
#' @description
#' Save combined data into a versioned Excel file to avoid overwriting.
#'
#' @param combined_data The combined observation data (data.frame).
#' @param excel_data_trial_path Path to the original Excel file (used as naming base).
#'
#' @return The full path of the newly saved versioned Excel file.
#' @export
save_data_to_excel<- function(combined_data, excel_data_trial_path) {
  # Extraire le chemin, nom et extension
  dir <- dirname(excel_data_trial_path)
  base <- tools::file_path_sans_ext(basename(excel_data_trial_path))
  ext <- tools::file_ext(excel_data_trial_path)

  # Générer un nom de fichier versionné
  i <- 1
  repeat {
    new_file <- file.path(dir, paste0(base, "_v", i, ".", ext))
    if (!file.exists(new_file)) break
    i <- i + 1
  }

  # Copier le fichier original
  success <- file.copy(from = excel_data_trial_path, to = new_file, overwrite = FALSE)
  if (!success) stop("❌ Unable to create versioned copy of Excel file.")

  # Charger le fichier copié
  wb <- openxlsx2::wb_load(new_file)

  # Supprimer l'ancienne feuille "data" si elle existe
  if ("data" %in% wb$sheet_names) {
    wb$remove_worksheet("data")
  }

  # Ajouter les nouvelles données dans une feuille "data"
  wb$add_worksheet("data")
  wb$add_data_table(sheet = "data", x = combined_data)
  wb$set_active_sheet("data")

  # Sauvegarder le fichier copié avec les nouvelles données
  wb$save(new_file)

  message(paste0("✅ Versioned Excel file saved as: ", new_file))
  return(new_file)
}

#' @description
#' Prepares the Excel file by copying the template if necessary.
#'
#' @param self The UserData object.
#' @param directory Path to store the file
#' @param filename name of the file ending with .xlsx
#'
#' @return None. Updates self$excel_data_trial if needed.
#' @export
prepare_excel_model <- function(self, directory = NULL, filename = NULL) {
  # Si le fichier trial n’existe pas ou est invalide
  if (is.null(self$excel_data_trial) || !file.exists(self$excel_data_trial)) {
    message("📁 Creating the trial Excel file from the blank template...")

    # Définir le nom de fichier
    if (is.null(filename)) {
      filename <- paste0(tools::file_path_sans_ext(basename(self$excel_model)), "_copie.xlsx")
    }

    # Définir le chemin complet
    if (is.null(directory)) {
      directory <- getwd()  # répertoire courant
    }

    full_path <- file.path(directory, filename)

    # Copier le modèle
    success <- file.copy(from = self$excel_model, to = full_path, overwrite = TRUE)

    if (success) {
      self$excel_data_trial <- full_path
      message(paste("✅ Trial Excel created at:", full_path))
    } else {
      stop("❌ Failed to create the trial Excel file.")
    }
  }
}

#' @description
#' Reads the 'placette' and 'modalite' sheets from the Excel trial file and stores them.
#'
#' @param self The UserData object.
#'
#' @return None. Updates self$plot_desc and self$moda_desc.
#' @description
#' Reads metadata sheets from the Excel trial file and stores them in metadata.
#'
#' @param self An instance of the UserData R6 class.
#' @export
load_metadata_sheets <- function(self) {
  wb_trial <- openxlsx2::wb_load(self$excel_data_trial)

  # Read and store 'placette' sheet
  if ("placette" %in% wb_trial$sheet_names) {
    placette_data <- openxlsx2::wb_read(wb_trial, sheet = "placette")
    placette_data <- placette_data[rowSums(is.na(placette_data) | placette_data == "") != ncol(placette_data), ]

    if (nrow(placette_data) > 0) {
      self$add_metadata("plot_desc", placette_data)
      message("✅ Sheet 'placette' loaded into metadata$placette.")
    } else {
      message("⚠️ Sheet 'placette' is empty.")
    }
  } else {
    message("ℹ️ Sheet 'placette' not found.")
  }

  # Read and store 'modalite' sheet
  if ("modalite" %in% wb_trial$sheet_names) {
    modalite_data <- openxlsx2::wb_read(wb_trial, sheet = "modalite")
    modalite_data <- modalite_data[rowSums(is.na(modalite_data) | modalite_data == "") != ncol(modalite_data), ]

    if (nrow(modalite_data) > 0) {
      self$add_metadata("moda_desc", modalite_data)
      message("✅ Sheet 'modalite' loaded into metadata$modalite.")
    } else {
      message("⚠️ Sheet 'modalite' is empty.")
    }
  } else {
    message("ℹ️ Sheet 'modalite' not found.")
  }
}

# Function to get and save the template Excel file
#' get_template_excel
#'
#' @param destination_path the file path where the template will be saved
#'
#' @returns
#' @export
#'
#' @examples
get_template_excel <- function(destination_path=NULL) {
  # Construct the path to the file in the package
  file_path <- system.file("extdata","template.xlsx",package="startbox")

  # Check if the file exists
  if (file.exists(file_path)) {
    if (!is.null(destination_path))
    {
    # Copy the file to the destination path specified by the user
    file.copy(file_path, destination_path)
    # Return a success message
    message("The file has been successfully saved to the specified location.")
    }
    else {
      # Construct the path to the user's Downloads directory
      download_path <- file.path(path.expand("~"), "modele_standard.xlsx")

      # Copy the file to the Downloads directory
      file.copy(file_path, download_path)

      # Return a success message
      message(paste("The file has been successfully saved to", download_path))
    }
  } else {
    # Return an error message if the file does not exist
    stop("The specified file does not exist in the inst/extdata directory of the package.")
  }
}

#' Import "data_*" sheets from an Excel file into a `user_data` object
#'
#' @param self The `user_data` R6 object
#' @param filepath Path to the Excel file to import
#'
#' @return Updates `self$obs_data` with the newly imported sheets
#' @export
load_data_sheets <- function(self, filepath) {
  if (!file.exists(filepath)) {
    stop("❌ File not found: ", filepath)
  }
  
  wb <- openxlsx2::wb_load(filepath)
  sheets <- wb$sheet_names
  
  data_sheets <- sheets[grepl("^data_", sheets)]
  if (length(data_sheets) == 0) {
    message("ℹ️ No sheets starting with 'data_' found in file.")
    return(invisible(NULL))
  }
  
  for (sheet in data_sheets) {
    df <- tryCatch({
      openxlsx2::wb_to_df(wb, sheet = sheet)
    }, error = function(e) {
      warning(paste("⚠️ Could not read sheet:", sheet))
      return(NULL)
    })
    
    if (is.null(df)) next
    
    # Nettoyage du nom : on supprime les caractères interdits (par sécurité)
    safe_sheet <- gsub("[:\\\\/*?\\[\\]]", "_", sheet)
    
    # Ajoute une mention si la feuille existait déjà
    if (safe_sheet %in% names(self$obs_data)) {
      message("🔁 Sheet already loaded: ", safe_sheet, " → replaced.")
    } else {
      message("✅ New sheet loaded: ", safe_sheet)
    }
    
    self$obs_data[[safe_sheet]] <- df
    
    
  }
  
  loaded_sheets <- names(self$obs_data)
  self$log_trace(
    operation = "import",
    filename = paste(loaded_sheets, collapse = ", "))
  
  invisible(self$obs_data)
}

#' Export observation sheets into a new Excel file version
#'
#' @description
#' This function exports all observation data (`obs_data`) from a `user_data` object into a new Excel file.
#' It preserves the original sheets and replaces/adds sheets named "data_XXX" accordingly.
#' The new file is saved in the Downloads folder with an incremented version name.
#'
#' @param self The `user_data` R6 object
#'
#' @return The full path of the exported Excel file (invisible)
#' @export
export_data_sheets <- function(self) {
  if (is.null(self$excel_data_trial) || !file.exists(self$excel_data_trial)) {
    stop("❌ No Excel file loaded in user_data.")
  }
  
  wb <- openxlsx2::wb_load(self$excel_data_trial)
  
  for (i in seq_along(self$obs_data)) {
    original_name <- names(self$obs_data)[i]
    df <- self$obs_data[[i]]
    
    sheetname <- if (grepl("^data_", original_name)) {
      original_name
    } else {
      paste0("data_", tools::file_path_sans_ext(basename(original_name)))
    }
    
    if (sheetname %in% wb$sheet_names) {
      openxlsx2::wb_remove_worksheet(wb, sheet = sheetname)
      message("🔁 Sheet replaced: ", sheetname)
    }
    
    wb$add_worksheet(sheetname)
    wb$add_data_table(sheet = sheetname, x = df)
    print(wb$sheet_names)
    message("✅ Sheet added: ", sheetname)
  }
  
  
  # Horodatage au format 2025-05-15_16h22
  timestamp <- format(Sys.time(), "%Y-%m-%d_%Hh%M")
  
  base_path <- normalizePath(self$excel_data_trial)
  base_file <- basename(base_path)
  name_no_ext <- tools::file_path_sans_ext(base_file)
  ext <- tools::file_ext(base_file)
  output_dir <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  
  new_filename <- file.path(output_dir, paste0(name_no_ext, "_", timestamp, ".", ext))
  
  wb$save(new_filename)
  print(wb$sheet_names)
  message("✅ New Excel file saved at: ", new_filename)
  
  added_sheets <- names(self$obs_data)
  self$log_trace(
    operation = "export",
    filename = paste(added_sheets, collapse = ", "))
  
  invisible(new_filename)
}

#' Wrapper to load all data to do graphs
#'
#' This function first imports all `data_*` sheets from the Excel file into the `obs_data` 
#' Loads the metadata if the plot and method sheets are filled in.
#'
#' @param self A `user_data` object (R6 class) containing the path to the Excel file.
#'
#' @return Invisibly returns invisible return, but file data loaded
#' @export
wrapper_data <- function(self) {
  # Step 1: import the data_* sheets (update self$obs_data)
  load_data_sheets(self, self$excel_data_trial)
  
  #Step 2 : import the "placette" and "modalite" sheets (update self$metadata)
  load_metadata_sheets(self)
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
standardise_data <- function(input_data, lookup_table = c("Mildiou_Feuille" = "PM_LEAF_PC","Mildiou_Grappe"  = "PM_BER_PC","Oidium_Grappe"   = "UN_BER_PC","Oidium_Feuille"  = "UN_LEAF_PC","Stade.phénologique" = "bbch_stage","Stade.phenologique" = "bbch_stage","Stade phenologique" = "bbch_stage","Placette" = "plot_id","Bloc" = "block_id","Date" = "observation_date","Code.essai" = "experiment_id")) {
  require(dplyr)
  require(tidyr)
  require(readr)
  require(readxl)
  require(tools)
  
  
  # --- Si c'est déjà un data.frame, on continue ---
  if (is.data.frame(input_data)) {
    df <- input_data
  } else if (is.character(input_data)) {
    # Lire les données
    ext <- tools::file_ext(input_data)
    
    # Lire les données selon l'extension
    if (ext == "csv") {
      df <- read.csv2(file = input_data, fileEncoding = "latin1")
    } else if (ext == "xlsx") {
      df <- readxl::read_excel(input_data, sheet = "data")
    }
  } else {
    stop("❌ Format non supporté. Utilisez un fichier .csv ou .xlsx.")
  }
  
  # Vérifier si les colonnes Maladie, Organe et Valeur sont présentes
  if (all(c("Maladie", "Organe", "Valeur") %in% names(df))) {
    df <- df %>%
      tidyr::pivot_wider(names_from = c(Maladie, Organe), values_from = Valeur)
  }
  
  # Renommer les colonnes selon le lookup_table
  noms_avant <- names(df)
  noms_apres <- ifelse(noms_avant %in% names(lookup_table), lookup_table[noms_avant], noms_avant)
  names(df) <- noms_apres
  
  # Formater la BBCH si présente sans dupliquer "BBCH"
  if ("bbch_stage" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(bbch_stage = ifelse(grepl("^BBCH", bbch_stage), bbch_stage, paste("BBCH", bbch_stage)))
  }
  
  # Réorganiser les colonnes : standardisées d'abord, autres ensuite
  colonnes_standard <- unname(lookup_table)   # ex: plot_id, block_id, etc.
  colonnes_presentes <- names(df)
  colonnes_autres <- setdiff(colonnes_presentes, colonnes_standard)
  df <- df[, c(intersect(colonnes_standard, colonnes_presentes), colonnes_autres)]
  
  # Ajouter xp_trt_code à partir de plot_id
  if ("plot_id" %in% names(df)) {
    df$xp_trt_code <- dplyr::case_when(
      grepl("^TNT", toupper(df$plot_id)) ~ "TNT",
      stringr::str_detect(df$plot_id, "[0-9]+") ~ stringr::str_extract(df$plot_id, "[0-9]+"),
      TRUE ~ NA_character_
    )
  }
  
  
  return(df)
}





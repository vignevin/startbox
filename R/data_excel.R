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
read_metadata_sheets <- function(self) {
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




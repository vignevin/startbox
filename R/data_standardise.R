#' @title Check if a CSV file is in TopVigne format
#'
#' @description
#' This function reads a .csv file and checks whether it contains the minimum
#' required columns to be considered a valid TopVigne file.
#'
#' @param filepath Path to the .csv file to check.
#'
#' @return Logical. Returns TRUE if the file is valid. Stops execution with an error message otherwise.
#'
#' @export
check_topvigne_csv <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("❌ Le fichier n'existe pas : ", filepath)
  }

  if (tools::file_ext(filepath) != "csv") {
    stop("❌ Le fichier doit être au format .csv.")
  }

  df <- utils::read.csv2(file = filepath, fileEncoding = "latin1")

  colonnes_attendues <- c("Maladie", "Organe", "Valeur", "Placette", "Bloc")
  colonnes_manquantes <- setdiff(colonnes_attendues, names(df))

  if (length(colonnes_manquantes) > 0) {
    stop("❌ Le fichier ne semble pas être un fichier TopVigne valide. Colonnes manquantes : ",
         paste(colonnes_manquantes, collapse = ", "), ".")
  }

  return(TRUE)
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
standardise_topvigne_csv <- function(filepath,
                                     lookup_table = c(
                                       "Mildiou_Feuille"    = "PM_LEAF_PC",
                                       "Mildiou_Grappe"     = "PM_BER_PC",
                                       "Oidium_Grappe"      = "UN_BER_PC",
                                       "Oidium_Feuille"     = "UN_LEAF_PC",
                                       "Stade.phénologique" = "bbch_stage",
                                       "Stade.phenologique" = "bbch_stage",
                                       "Stade phenologique" = "bbch_stage",
                                       "Placette"           = "plot_id",
                                       "Bloc"               = "plot_block",
                                       "Date"               = "observation_date",
                                       "Code.essai"         = "experiment_id")) {

  check_topvigne_csv(filepath)

  df <- utils::read.csv2(file = filepath, fileEncoding = "latin1")

  if (all(c("Maladie", "Organe", "Valeur") %in% names(df))) {
    df <- df %>%
      tidyr::pivot_wider(names_from = c(Maladie, Organe), values_from = Valeur)
  }

  noms_avant <- names(df)
  noms_apres <- ifelse(noms_avant %in% names(lookup_table), lookup_table[noms_avant], noms_avant)
  names(df) <- noms_apres

  if ("bbch_stage" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(bbch_stage = ifelse(grepl("^BBCH", bbch_stage), bbch_stage, paste("BBCH", bbch_stage)))
  }

  colonnes_standard <- unname(lookup_table)
  colonnes_presentes <- names(df)
  colonnes_autres <- setdiff(colonnes_presentes, colonnes_standard)
  df <- df[, c(intersect(colonnes_standard, colonnes_presentes), colonnes_autres)]

  # Remove unwanted column "X." if still present
  df <- df[, !names(df) %in% c("X.","X")]

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
standardise_data <- function(input_data, lookup_table = c("Mildiou_Feuille" = "PM_LEAF_PC","Mildiou_Grappe"  = "PM_BER_PC","Oidium_Grappe"   = "UN_BER_PC","Oidium_Feuille"  = "UN_LEAF_PC","Stade.phénologique" = "bbch_stage","Stade.phenologique" = "bbch_stage","Stade phenologique" = "bbch_stage","Placette" = "plot_id","Bloc" = "block_id","Date" = "observation_date","Code.essai" = "experiment_id")) {

  if (is.data.frame(input_data)) {
    df <- input_data
  } else if (is.character(input_data)) {
    ext <- tools::file_ext(input_data)

    # Lire les données selon l'extension
    if (ext == "csv") {
      df <- utils::read.csv2(file = input_data, fileEncoding = "latin1")
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


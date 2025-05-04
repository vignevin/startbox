## R6 tests

### R6 class definition
# see https://thinkr.fr/au-menu-du-jour-r6-partie-1/
# see https://linogaliana.gitlab.io/collaboratif/package.html

library(openxlsx2)
library(dplyr)

#----Classe R6----
# create R6 class to store user_data
user_data <- R6::R6Class(
  "UserData",
  public = list(
    # excel template path
    excel_model = NULL,
    # data_trial excel
    excel_data_trial =NULL,
    # combined data
    combined_data = NULL,
    # obs_data is the list of dataframes with observation data. each list item has the name of the source file
    # if the item is a sheet of an excel file, the name will be filename:sheetname
    obs_data = list(),
    metadata = list(),


    #' @description
        #' create a new "UserData" object
        #'
        #'
    #'
    #' @param excel_model excel template path
    #' @param excel_data_trial excel data trial path
    #'
    #' @returns a  "UserData" object
    initialize = function(excel_model = NULL, excel_data_trial =NULL){
      self$excel_model <- excel_model
      self$excel_data_trial <- excel_data_trial
    },

    #' METTRE SKELETON
    add_metadata = function(name, value) {
      if (!is.character(name) || length(name) != 1) {
        stop("Name must be a single character string.")
      }
      self$metadata[[name]] <- value
    },



    # Add or update observation dataset
    #' @description
        #' Internal function to add or update a dataset of observations
    #'
    #' @param name the name of the observation to add
    #' @param df dataframe with observation data.
    #'
    #' @returns updated UserData

    add_obs = function(name, df) {
      if (name %in% names(self$obs_data)) {
        message(paste("Updating element:", name))
      } else {
        message(paste("Adding a new element:", name))
      }

      # Add the columns prov_name and prov_date
      df$prov_name <- as.character(name)
      df$prov_date <- format(Sys.Date(), "%d/%m/%Y")

      self$obs_data[[name]] <- df
    },

    # Method for displaying elements
    show_obs_data = function() {
      lapply(self$obs_data, head)
    },

    #' @description
    #' Combines all loaded observation datasets, applies type harmonization, and merges them with an existing Excel trial file.
    #' If the Excel trial file does not exist, it is created from a template. Also reads available metadata sheets.
    #'
    #' @return A combined `data.frame` of all observations, stored in `self$combined_data` and returned invisibly.
    combine_data_obs = function() {
      if (length(self$obs_data) == 0) {
        message("No data to combine.")
        return(NULL)
      }
      # Harmonize all observation datasets
      self$obs_data <- harmonize_all_obs_data(self$obs_data)
      # Combine and reorder observations
      combined <- combine_and_reorder_obs(self$obs_data)
      # Prepare Excel trial file if necessary
      prepare_excel_model(self, filename = "testnomfichier.xlsx", directory = "C:/Users/hmaire.VIGNEVIN/OneDrive - IFV/Bureau")
      # Load placette and modalite sheets if present
      read_metadata_sheets(self)

      # Merge with existing 'data' sheet if it exists
      wb <- openxlsx2::wb_load(self$excel_data_trial)
      combined <- merge_with_existing_data(wb, combined)
      combined <- combined[rowSums(is.na(combined) | combined == "") != ncol(combined), ]


      self$combined_data <- combined
      return(combined)
    },

    #---- A METTRE EN DEHORS DE LA CLASSE----
    #' @description
    #' Merges observation data with metadata sheets ('placette' and 'modalite') if available.
    #' The function first joins the plot and treatment metadata, then links this to the combined observation data
    #' based on the 'plot_id' column.
    #'
    #' @return A data.frame with observation data joined with plot and treatment metadata.
    #' If no observation data is present, returns only the joined metadata.
    prepare_final_data = function() {
      if (is.null(self$metadata$plot_desc) || is.null(self$metadata$moda_desc)) {
        message("❌ plot_desc or moda_desc missing.")
        return(NULL)
      }

      # Step 1 — Join plot + modality
      self$metadata$plot_desc$factor_level_code <- as.character(self$metadata$plot_desc$factor_level_code)
      self$metadata$moda_desc$xp_trt_code <- as.character(self$metadata$moda_desc$xp_trt_code)

      df_plot_moda <- dplyr::left_join(
        self$metadata$plot_desc,
        self$metadata$moda_desc,
        by = c("factor_level_code" = "xp_trt_code")
      )

      # Explicitly add xp_trt_code
      df_plot_moda$xp_trt_code <- df_plot_moda$factor_level_code

      # Step 2 — Add observation data
      if (length(self$obs_data) == 0) {
        message("⚠️ No observation data to join. Returning only plot + modality.")
        return(df_plot_moda)
      }

      if (is.null(self$combined_data)) {
        message("❌ Combined data is not ready yet. Call combine_data_obs() first.")
        return(NULL)
      }

      df_obs <- self$combined_data
      df_obs$plot_id <- as.character(df_obs$plot_id)
      df_plot_moda$plot_id <- as.character(df_plot_moda$plot_id)

      # Final merge of observations + plot + modality
      df_final <- dplyr::left_join(df_obs, df_plot_moda, by = "plot_id")

      return(df_final)
    }
  )
)

#----Fonctions en dehors de la classe R6----

#' @description
#' Save combined data into the 'data' sheet of an Excel file.
#'
#' @param combined_data The combined observation data (data.frame).
#' @param excel_data_trial_path Path to the Excel file (.xlsx).
#'
#' @return None. Writes and saves into Excel.
save_data_to_excel <- function(combined_data, excel_data_trial_path) {
  wb <- openxlsx2::wb_load(excel_data_trial_path)

  # Remove existing "data" sheet if present
  if ("data" %in% wb$sheet_names) {
    wb$remove_worksheet("data")
  }

  # Now create a new one cleanly
  wb$add_worksheet("data")
  wb$add_data_table(sheet = "data", x = combined_data)
  wb$set_active_sheet("data")

  wb$save(excel_data_trial_path)

  message("✅ Combined data has been saved into the 'data' sheet.")
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

#' @description
#' Combines all observation datasets into a single dataframe and reorders columns.
#'
#' @param obs_data A list of harmonized observation datasets.
#'
#' @return A combined and reordered dataframe.
combine_and_reorder_obs <- function(obs_data) {
  combined <- dplyr::bind_rows(obs_data)

  template_cols <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
  valid_cols <- intersect(template_cols, names(combined))
  ordered_cols <- c(valid_cols, setdiff(names(combined), valid_cols))

  combined <- combined[, ordered_cols, drop = FALSE]
  return(combined)
}


#' @description
#' Prepares the Excel file by copying the template if necessary.
#'
#' @param self The UserData object.
#' @param directory Path to store the file
#' @param filename name of the file ending with .xlsx
#'
#' @return None. Updates self$excel_data_trial if needed.
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

#' @description
#' Merges new combined data with existing data in the Excel file.
#'
#' @param wb The openxlsx2 workbook object.
#' @param combined The newly combined data to insert.
#'
#' @return A dataframe containing old data + new combined data.
merge_with_existing_data <- function(wb, combined) {
  if ("data" %in% wb$sheet_names) {
    old_data <- wb_read(wb, sheet = "data")

    # Harmonize sensitive columns as character
    columns_to_character <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
    for (col in columns_to_character) {
      if (col %in% names(old_data)) old_data[[col]] <- as.character(old_data[[col]])
      if (col %in% names(combined)) combined[[col]] <- as.character(combined[[col]])
    }

    # *_PC columns → numeric
    pc_cols <- grep("_PC$", names(old_data), value = TRUE)
    old_data[pc_cols] <- lapply(old_data[pc_cols], function(col) as.numeric(as.character(col)))

    if ("prov_name" %in% names(old_data) && "prov_name" %in% names(combined)) {
      # Files already present in the old dataset
      old_prov_files <- unique(old_data$prov_name)
      # Files present in the new imports
      new_prov_files <- unique(combined$prov_name)
      # Files to update: those existing in both
      prov_names_to_update <- intersect(old_prov_files, new_prov_files)
      if (length(prov_names_to_update) > 0) {
        message(paste0("🔁 Updating data for: ", paste(prov_names_to_update, collapse = ", ")))
        old_data <- old_data[!(old_data$prov_name %in% prov_names_to_update), ]
      }
    }

    # Finally, concatenate
    combined <- dplyr::bind_rows(old_data, combined)

    # Remove the old sheet
    wb$remove_worksheet("data")
  }

  return(combined)
}

#' @description
#' Harmonizes the types of columns in a dataframe according to a type mapping.
#'
#' @param df The dataframe to harmonize.
#' @param types_map Optional: predefined type mapping (otherwise it will load from a CSV).
#' @param dictionary_path Optional: path to the CSV dictionary file if types_map is not provided.
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

#----Fonction Graphiques----

#' @title Plot a Heatmap of Experimental Data
#'
#' @description
#' Generates a heatmap based on experimental observation data,
#' using `plot_x` and `plot_y` coordinates and coloring according to a selected disease variable (_PC columns).
#'
#' @param data A dataframe containing at least 'plot_x', 'plot_y', *_PC variable (e.g., PM_LEAF_PC) and a column plot_id
#' @param titre (optional) Title of the plot. If NULL, a default title based on the variable is used.
#' @param echelle (optional) Maximum value for the fill color scale. If NULL, an automatic scale is calculated.
#'
#' @return A ggplot2 heatmap object.
#'
#' @details
#' - Aggregates values by `plot_id`, taking the mean when multiple observations exist.
#' - If no *_PC variable is found in the data, the function will stop with an error.
#' - Useful for visualizing spatial patterns of disease in experimental trials.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' df <- mydata$prepare_final_data()
#' plot_xpheat(df)
#'
#'
# METTRE VARIABLE EN PARAMETRE ENTREE
# AJOUTER PLOT_ID DANS LE CHECK
# SUPPRIMER XP_TRT DU GROUP_BY
plot_xpheat <- function(data, titre = NULL, echelle = NULL) {
  library(ggplot2)
  library(dplyr)

  # --- Vérification pos_x et pos_y ---
  if (!all(c("plot_x", "plot_y") %in% names(data))) {
    stop("Les colonnes 'plot_x' et 'plot_y' sont nécessaires pour tracer une heatmap.")
  }

  # --- Trouver la bonne variable ---
  known_vars <- c("PM_LEAF_PC", "PM_BER_PC", "UN_LEAF_PC", "UN_BER_PC")
  candidate_vars <- intersect(known_vars, names(data))

  if (length(candidate_vars) == 0) {
    stop("Aucune variable _PC trouvée dans vos données.")
  }

  variable <- candidate_vars[1]

  # Étape 1 : on calcule la moyenne par placette (plot_id)
  data_agg <- data %>%
    group_by(plot_id, plot_x, plot_y, xp_trt_code) %>%
    summarise(Valeurs = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")

  print(data_agg)
  print(str(data_agg))

  if (nrow(data_agg) == 0) {
    stop("❌ Aucune donnée agrégée trouvée : vérifiez que les valeurs ne sont pas toutes NA, et que la variable existe bien.")
  }


  # --- Echelle automatique ---
  if (is.null(echelle)) {
    max_val <- max(data_agg$Valeurs, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)
  }

  if (is.null(titre)) titre <- paste("Heatmap -", variable)

  # --- Graphique ---
  p <- ggplot(data_agg, aes(x = plot_x, y = plot_y, fill = Valeurs)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = plot_id), color = "white", size = 4) +
    labs(
      title = titre,
      fill = "Valeur (%)",
      caption = "IFV+"
    ) +
    scale_fill_gradient(low = "yellow", high = "red", limits = c(0, echelle),name = "Valeur Moyenne Intensité (%)") +
    scale_y_continuous(breaks = seq(min(data$plot_y), max(data$plot_y), by = 1)) +
    coord_fixed() +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )

  return(p)
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


#' calcuate frequency in percent of values >0from a numeric vector
#'
#' @param vecteur
#'
#' @returns a numeric value of frequency, in percent
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 0, 5, 6, 0)
#' frequency(vec)
frequency <- function(vecteur) {
  # Check if the input is a numeric vector
  if (!is.numeric(vecteur)) {
    stop("Input must be a numeric vector.")
  }
  # Calculate the frequency of values greater than zero
  freq <- length(vecteur[vecteur > 0]) * 100 / length(vecteur)
  return(freq)
}

#' calcul efficacy
#'
#' @param value numeric value
#' @param value_tnt numeric value of TNT
#'
#' @returns
#' @export
#'
#' @examples
efficacy <- function(value,value_tnt)
{
  eff <- 100-((value*100/value_tnt))
  return(eff)
}


#' Barplot résumé pour une expérimentation
#'
#' @description
#' Cette fonction génère automatiquement un graphique en barres à partir de données d'observations ou expérimentales.
#'
#' @param data Données brutes contenant au moins plot_id, et une variable "_PC".
#' @param type Type de graphique ("Frequence", "Intensite", ou "Both") par défaut "Both".
#' @param titre Titre principal du graphique (optionnel).
#' @param echelle Limite supérieure de l'axe y. Si NULL, ajustement automatique.
#' @param couleur_bars Couleur de remplissage des barres.
#' @param bar_width Largeur des barres.
#'
#' @return Un graphique en barre
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' # Exemple d'utilisation
#' plot_xpbar(my_data)
#
# AJOUTER VARIABLES EN ENTREE, AVEC POSSIBILITE DE METTRE UNE OU DEUX VARIABLES
# PARAM OPTION POUR COLORER LA BORDURE DU TNT EN ROUGE
# IL FAUT QUE DANS DATA IL Y AIT UNE VARIABLE GROUPE : MODALITE PAR EXEMPLE = PARAM ENTREE
# EST-CE QUE DATA EN ENTREE NE SERAIT PAS DEJA UN DF SUMMARY ? -> NON MAIS PREVOIR DE CHOISIR LA STAT (MEAN, MEDIAN, SUM...)
# TYPE = VARIABLE...PAS CLAIR
# SORTIR FONCTION SPLIT ID ET SI POSSIBLE LA RENDRE PLUS GENERIQUE -> voir remove_block_code
plot_xpbar <- function(data, type = c("Both", "Intensite", "Frequence"),titre = NULL, echelle = NULL, couleur_bars = "#00AB50", bar_width = 0.7) {
  library(ggplot2)
  library(dplyr)

  type <- match.arg(type)

  if (!"plot_id" %in% names(data)) {
    stop("La colonne 'plot_id' est manquante dans vos données.")
  }

  data$plot_id <- trimws(data$plot_id)

  # --- Trouver la bonne variable parmi les variables autorisées ---
  known_vars <- c("PM_LEAF_PC", "PM_BER_PC", "UN_LEAF_PC", "UN_BER_PC")
  candidate_vars <- intersect(known_vars, names(data))

  if (length(candidate_vars) == 0) {
    stop("Aucune variable valide (_PC) n'a été trouvée dans vos données.")
  }
  if (length(candidate_vars) > 1) {
    message("Plusieurs variables trouvées : ", paste(candidate_vars, collapse = ", "))
    message("La première variable détectée est utilisée : ", candidate_vars[1])
  }

  variable <- candidate_vars[1]  # Sélection automatique

  # --- Séparer plot_id ---
  split_plot_id <- function(pid) {
    if (grepl("^TNT", pid)) {
      xp_trt_code <- "TNT"
      block_id <- gsub("TNT", "", pid)
    } else {
      xp_trt_code <- gsub("[A-Za-z]", "", pid)
      block_id <- gsub("[0-9]", "", pid)
    }
    list(xp_trt_code = xp_trt_code, block_id = block_id)
  }

  temp_split <- lapply(data$plot_id, split_plot_id)

  data <- data.frame(
    plot_id = data$plot_id,
    xp_trt_code = sapply(temp_split, `[[`, "xp_trt_code"),
    block_id = sapply(temp_split, `[[`, "block_id"),
    bbch_stage = data$bbch_stage,
    observation_date = data$observation_date,
    Valeurs = as.numeric(data[[variable]]),
    stringsAsFactors = FALSE
  )

  # --- Créer Frequence et Intensite ---
  data_fq <- data
  data_fq$Type <- "Frequence"
  data_fq$Valeurs <- ifelse(data_fq$Valeurs > 0, 100, 0)

  data_int <- data
  data_int$Type <- "Intensite"

  data_all <- bind_rows(data_fq, data_int)

  Resume <- data_all %>%
    group_by(xp_trt_code, Type) %>%
    summarise(
      mean_resultat = mean(Valeurs, na.rm = TRUE),
      sd_resultat = sd(Valeurs, na.rm = TRUE),
      .groups = "drop"
    )

  if (type != "Both") {
    data_plot <- Resume %>% filter(Type == type)
    fill_color <- couleur_bars
  } else {
    data_plot <- Resume
    fill_color <- c("Frequence" = "#00AB50", "Intensite" = "#9966CC")
  }

  # --- Calcul automatique échelle si non spécifiée ---
  if (is.null(echelle)) {
    max_val <- max(data_plot$mean_resultat + data_plot$sd_resultat, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)  # un peu de marge au-dessus
  }

  if (is.null(titre)) titre <- paste("Graphique -", variable)

  # Préparer la couleur des contours
  data_plot$border_color <- ifelse(data_plot$xp_trt_code == "TNT", "red", "grey30")

  #Création du graphique
  p <- ggplot(data_plot, aes(x = xp_trt_code, y = mean_resultat, fill = Type)) +
    geom_bar(aes(color = border_color), stat = "identity", position = position_dodge(width = bar_width), width = bar_width, linewidth = 1) +
    geom_errorbar(
      data = data_plot %>% filter(sd_resultat > 0),
      aes(ymin = mean_resultat - sd_resultat, ymax = mean_resultat + sd_resultat),
      width = 0.2, position = position_dodge(width = bar_width)
    ) +
    geom_text(aes(label = round(mean_resultat, 1)),
              vjust = -0.5, size = 4, color = "black", position = position_dodge(width = bar_width)) +
    labs(
      title = titre,
      y = "Pourcentage (%)",
      caption = "IFV+"
    ) +
    ylim(0, echelle) +
    scale_fill_manual(values = fill_color) +
    scale_color_identity() +    # <<<<<< très important
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = if (type == "Both") "top" else "none"
    )

  return(p)
}


### preparation des donness

#' Title
#'
#' @param data a dataframe to resume
#' @param var_col character, the colname of the variable to plot
#' @param group_col the colname of the group
#' @param funs statistic to plot, a vector of one or two statistic. by default c("mean","frequency")
#'
#' @returns a dataframe ready for plotting
#' @export
#'
#' @examples
resume_data <- function(data, var_col, group_cols,
                        funs = list(intensite=mean,frequence=frequency)) {
  # convert column argument to symbol
  var <- dplyr::ensym(var_col)
  group_syms <- dplyr::syms(group_cols)

  # Determine the name for the new column
  new_col_name <- if ("type" %in% colnames(data)) "type2" else "type"

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
        dplyr::mutate(!!new_col_name := !!names(funs)[i])
    } else {
      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(value = funs[[i]]({{ var }}), .groups = "drop") %>%
        dplyr::mutate(!!new_col_name := !!names(funs)[i])
    }
    data_resume <- dplyr::bind_rows(data_resume,resume)
  }
  return(data_resume)
} #end function

###### Version 2

#' Barplot résumé pour une expérimentation
#'
#' @description
#' Cette fonction génère automatiquement un graphique en barres à partir de données d'observations ou expérimentales.
#'
#' @param data2plot a dataframe, the first col is used for x axis, col "value" for y axis and col "type" for fill.
#' @param echelle Limite supérieure de l'axe y. Si NULL, ajustement automatique.
#' @param couleur_bars Couleur de remplissage des barres.
#' @param bar_width bar width
#' @param option_border_tnt if true, the border color is set as red for TNT
#' @param ... other parameters for labs (title, x, y,fill)
#'
#' @return a barplot
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' # Exemple d'utilisation
#' plot_xpbar(my_data)
#
plot_xpbar2 <- function(data2plot,
                        echelle = NULL,
                        couleur_bars = c("#00AB50", "#9966CC"),
                        bar_width = 0.7,
                        option_border_tnt = TRUE,
                        ...) {
  library(ggplot2)
  library(dplyr)

  ## ?
  #type <- match.arg(type)

  # set col for x axis
  xcol = sym(colnames(data2plot)[1])

  # Préparer la couleur des contours
  data2plot$border_color <- "grey30"
  if(option_border_tnt) {
  data2plot$border_color <- ifelse(data2plot[,paste(xcol)] == "TNT", "red", "grey30")}

  # --- Calcul automatique échelle si non spécifiée ---
  if (is.null(echelle)) {
    max_val <- max(data2plot$value, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)  # un peu de marge au-dessus
  }

  p <- ggplot(data2plot, aes(x = !!xcol,
                             y = value, fill = type)) +
    geom_bar(color = data2plot$border_color,
             stat = "identity",
             position = position_dodge(width = bar_width),
             width = bar_width, linewidth = 0.5) +
    #geom_errorbar(
    #  data = data_plot %>% filter(sd_resultat > 0),
    #  aes(ymin = mean_resultat - sd_resultat, ymax = mean_resultat + sd_resultat),
    #  width = 0.2, position = position_dodge(width = bar_width)
    #) +
    geom_text(aes(label = round(value, 1)),
              vjust = -0.5, size = 4, color = "black", position = position_dodge(width = bar_width)) +
    labs(...) +
    ylim(0, echelle) +
    scale_fill_manual(values = couleur_bars) +
    scale_color_identity() +    # <<<<<< très important
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = if (length(unique(data2plot$type))>1) "right" else "none"
    )

  return(p)
}






#' Boxplot résumé pour une expérimentation
#'
#' @description
#' Cette fonction génère automatiquement un graphique en boîte à moustaches
#' (boxplot) à partir de données expérimentales brutes (parcelle, observation, maladie).
#'
#' @param data Données brutes contenant au moins plot_id, bbch_stage, observation_date et une variable "_PC".
#' @param titre Titre principal du graphique (optionnel).
#' @param echelle Limite supérieure de l'axe y (optionnel, sinon automatique).
#'
#' @return Un objet `ggplot2`.
#' @export
#'
#' @examples
#' plot_xpbox(my_data)
#'


plot_xpbox <- function(data, titre = NULL, echelle = NULL) {
  library(ggplot2)
  library(dplyr)

  if (!"plot_id" %in% names(data)) {
    stop("La colonne 'plot_id' est manquante dans vos données.")
  }

  data$plot_id <- trimws(data$plot_id)

  known_vars <- c("PM_LEAF_PC", "PM_BER_PC", "UN_LEAF_PC", "UN_BER_PC")
  candidate_vars <- intersect(known_vars, names(data))

  if (length(candidate_vars) == 0) {
    stop("Aucune variable _PC valide trouvée dans les données.")
  }
  if (length(candidate_vars) > 1) {
    message("Plusieurs variables _PC trouvées : ", paste(candidate_vars, collapse = ", "))
    message("La première variable détectée est utilisée : ", candidate_vars[1])
  }

  variable <- candidate_vars[1]

  # Séparer plot_id
  split_plot_id <- function(pid) {
    if (grepl("^TNT", pid)) {
      xp_trt_code <- "TNT"
      block_id <- gsub("TNT", "", pid)
    } else {
      xp_trt_code <- gsub("[A-Za-z]", "", pid)
      block_id <- gsub("[0-9]", "", pid)
    }
    list(xp_trt_code = xp_trt_code, block_id = block_id)
  }

  temp_split <- lapply(data$plot_id, split_plot_id)

  data <- data.frame(
    plot_id = data$plot_id,
    xp_trt_code = sapply(temp_split, `[[`, "xp_trt_code"),
    block_id = sapply(temp_split, `[[`, "block_id"),
    bbch_stage = data$bbch_stage,
    observation_date = data$observation_date,
    Valeurs = as.numeric(data[[variable]]),
    stringsAsFactors = FALSE
  )

  # Filtrer sur Intensité uniquement
  data$Type <- "Intensite"

  # Calcul automatique échelle
  if (is.null(echelle)) {
    max_val <- max(data$Valeurs, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)
  }

  if (is.null(titre)) {
    titre <- paste("Boxplot -", variable)
  }

  p <- ggplot(data, aes(x = xp_trt_code, y = Valeurs)) +
    geom_boxplot(
      aes(fill = xp_trt_code),
      alpha = 0.6, outlier.color = "red", outlier.shape = NA
    ) +
    stat_summary(
      fun = mean, geom = "point", shape = 4, size = 3, color = "black"
    ) +
    labs(
      title = titre,
      subtitle = paste("Date :", paste(unique(data$observation_date), collapse = ", ")),
      x = "Traitement",
      y = "Intensité (%)",
      caption = "IFV+"
    ) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, echelle)) +
    scale_fill_manual(
      values = rep("#00AB50", length(unique(data$xp_trt_code)))
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")

  return(p)
}



#----Éxécution du code----

### Creating an instance of the mydata class
mydata <- user_data$new(excel_model = "inst/extdata/template.xlsx")
mydata <- user_data$new(excel_data_trial = "misc/biovimed_teissonniere_2024.xlsx") # File already containing data

mydata$excel_model

### Adding an observation dataframe
# Reading the data frame
# This will be done by a Shiny function
myfilepath = "misc/teissonniere_dataF1_2024.csv"
data1 <- read.csv2(myfilepath)

# Add the data frame
mydata$add_obs(name = basename(myfilepath), df = data1)

myfilepath2 = "misc//teissonniere_dataF2_2024.csv"
data2 <- read.csv2(myfilepath2)

# Add the data frame
mydata$add_obs(name = basename(myfilepath2), df = data2)

myfilepath3 = "misc//teissonniere_dataF3_2024.csv"
data3 <- read.csv2(myfilepath3)
mydata$add_obs(name = basename(myfilepath3), df = data3)

myfilepath4 <- "misc//teissonniere_dataGrappes_2024.xlsx"
mysheet4 <- "teissonniere_dataG2_2024"
data4 <- openxlsx2::read_xlsx(myfilepath4, sheet = mysheet4)
mydata$add_obs(name = paste0(basename(myfilepath4),":",mysheet4), df = data4)

mysheet5 <- "teissonniere_dataG1_2024"
data5 <- openxlsx2::read_xlsx(myfilepath4, sheet = mysheet5)
mydata$add_obs(name = paste0(basename(myfilepath4),":",mysheet5), df = data5)


mydata$excel_data_trial

### fonctions a travailler
# 1. combiner des observations dans un seul fichier
# 2. exporter dans un fichier exCel modele si excel_data_trial est null

# 3. ajout d'un fichier excel modele
# 4. s'il existe, et si la feuille placette existe, importer la feuille placette dans plot_desc


mydata$combine_data_obs()
save_data_to_excel(mydata$combined_data, mydata$excel_data_trial)

mydata$plot_desc
mydata$moda_desc

mydata$show_obs_data()

df_complet <- mydata$prepare_final_data()
View(df_complet)

my_data1 <- read.csv2("misc/teissonniere_dataF1_2024.csv", sep = ";")

## add trt_code
my_data1$trt_code <- remove_block_code(my_data1$plot_id,blocks = c("A","B","C","D"))

data2 <- resume_data(my_data1, var_col = "PM_LEAF_PC", group_cols = c("plot_id","trt_code"),funs=list(intensité=mean,fréquence=frequency))
data3 <- resume_data(data2,var_col = "value",group_cols=c("trt_code","type"),funs=list(moyenne=mean))

plot_xpbar2(data2plot=data3,title="My graph",y="Pourcentage %",x="Modalité",fill="Variable")

plot_xpbar2(data2plot=data3 %>% filter(type=="intensité"),title="My graph",y="Pourcentage %",x="Modalité",fill="Variable")


plot_xpbar(my_data1,"Frequence")
plot_xpbox(my_data1,echelle = 15)
plot_xpheat(df_complet)

mydata$metadata
read_metadata_sheets(mydata)

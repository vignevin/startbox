## R6 tests

### R6 class definition
# see https://thinkr.fr/au-menu-du-jour-r6-partie-1/
# see https://linogaliana.gitlab.io/collaboratif/package.html

library(openxlsx2)
library(dplyr)


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
    #data.frame containing placette sheet data if filled in
    plot_desc = NULL,
    #data.frame containing modalite sheet data if filled in
    moda_desc = NULL,
    # obs_data is the list of dataframes with observation data. each list item has the name of the source file
    # if the item is a sheet of an excel file, the name will be filename_sheetname
    obs_data = list(),


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

      # Convert *_PC columns to numeric even if they are text
      pc_cols <- grep("_PC$", names(df), value = TRUE)
      df[pc_cols] <- lapply(df[pc_cols], function(col) as.numeric(as.character(col)))

      self$obs_data[[name]] <- df
    },

    # Method for displaying elements
    show_obs_data = function() {
      lapply(self$obs_data, head)
    },

    combine_data_obs = function() {
      if (length(self$obs_data) == 0) {
        message("No data to combine.")
        return(NULL)
      }

      self$obs_data <- harmonize_all_obs_data(self$obs_data)
      combined <- combine_and_reorder_obs(self$obs_data)

      prepare_excel_model(self)
      read_metadata_sheets(self)

      wb <- openxlsx2::wb_load(self$excel_data_trial)
      combined <- merge_with_existing_data(wb, combined)

      wb$add_worksheet("data")
      wb$add_data_table(sheet = "data", x = combined)
      wb$set_active_sheet("data")
      wb$save(self$excel_data_trial)

      self$combined_data <- combined
      message("✅ Combined data has been inserted into the 'data' sheet.")
      return(combined)
    },

    prepare_final_data = function() {
      if (is.null(self$plot_desc) || is.null(self$moda_desc)) {
        message("❌ plot_desc or moda_desc missing.")
        return(NULL)
      }

      # Step 1 — Join plot + modality
      self$plot_desc$factor_level_code <- as.character(self$plot_desc$factor_level_code)
      self$moda_desc$xp_trt_code <- as.character(self$moda_desc$xp_trt_code)

      df_plot_moda <- dplyr::left_join(
        self$plot_desc,
        self$moda_desc,
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

harmonize_all_obs_data <- function(obs_data) {
  lapply(obs_data, harmonize_column_types)
}

combine_and_reorder_obs <- function(obs_data) {
  combined <- dplyr::bind_rows(obs_data)

  template_cols <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
  valid_cols <- intersect(template_cols, names(combined))
  ordered_cols <- c(valid_cols, setdiff(names(combined), valid_cols))

  combined <- combined[, ordered_cols, drop = FALSE]
  return(combined)
}

prepare_excel_model <- function(self) {
  if (is.null(self$excel_data_trial) || !file.exists(self$excel_data_trial)) {
    message("Copying the blank template into excel_data_trial")
    trial_path <- tempfile(fileext = ".xlsx")
    file.copy(from = self$excel_model, to = trial_path, overwrite = TRUE)
    self$excel_data_trial <- trial_path
  }
}

read_metadata_sheets <- function(self) {
  wb_trial <- wb_load(self$excel_data_trial)

  if ("placette" %in% wb_trial$sheet_names) {
    placette_data <- wb_read(wb_trial, sheet = "placette")
    placette_data <- placette_data[rowSums(is.na(placette_data) | placette_data == "") != ncol(placette_data), ]
    if (nrow(placette_data) > 0) {
      message("✅ Data from sheet 'placette' loaded into plot_desc.")
      self$plot_desc <- placette_data
    } else {
      message("No data found in the 'placette' sheet.")
    }
  }

  if ("modalite" %in% wb_trial$sheet_names) {
    modalite_data <- wb_read(wb_trial, sheet = "modalite")
    modalite_data <- modalite_data[rowSums(is.na(modalite_data) | modalite_data == "") != ncol(modalite_data), ]
    if (nrow(modalite_data) > 0) {
      message("✅ Data from sheet 'modalite' loaded into moda_desc.")
      self$moda_desc <- modalite_data
    } else {
      message("No data found in the 'modalite' sheet.")
    }
  }
}

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

harmonize_column_types <- function(df, types_map = NULL) {

  if (is.null(types_map)) {
    types_df <- read.csv2("inst/extdata/star_dictionary.csv", stringsAsFactors = FALSE)
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

  # *_PC columns → numeric
  pc_cols <- grep("_PC$", names(df), value = TRUE)
  for (col in pc_cols) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }

  return(df)
}



### Creating an instance of the mydata class
mydata <- user_data$new(excel_model = "inst/extdata/template.xlsx")

mydata <- user_data$new(excel_data_trial = "misc//biovimed_teissonniere_2024.xlsx") # File already containing data

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

mydata$show_obs_data()

mydata$excel_data_trial

### fonctions a travailler
# 1. combiner des observations dans un seul fichier
# 2. exporter dans un fichier exCel modele si excel_data_trial est null

# 3. ajout d'un fichier excel modele
# 4. s'il existe, et si la feuille placette existe, importer la feuille placette dans plot_desc


#---- Combiner des observations dans un seul fichier ----

mydata$combine_data_obs()

#----

mydata$plot_desc
mydata$moda_desc


df_complet <- mydata$prepare_final_data()
View(df_complet)




















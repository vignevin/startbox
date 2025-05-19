# create R6 class to store user_data
#' @title User Data R6 Class
#' @export
user_data <- R6::R6Class(
  "UserData",
  public = list(
    # excel template path
    excel_model = NULL,
    # data_trial excel
    excel_data_trial = NULL,
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
    initialize = function(excel_model = NULL, excel_data_trial = NULL) {
      if (is.null(excel_model)) {
        self$excel_model <- system.file("extdata", "template.xlsx", package = "startbox")
      } else {
        self$excel_model <- excel_model
      }
      self$excel_data_trial <- excel_data_trial
      
    },
    
    #' @description
    #' Adds an element to the `metadata` list stored in the R6 object.
    #'
    #' @param name A single character string specifying the name of the element to add.
    #' @param value The object to add (e.g., a data.frame, list, or other metadata).
    #'
    #' @return None. Modifies the `metadata` list in-place.
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
    }
  )
)

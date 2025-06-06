#' @title User Data R6 Class
#'
#' @description
#' This R6 class is designed to manage experimental data, including observation sheets, metadata,
#' and traceability logs. It provides methods to import, merge, and export data from Excel files
#' while tracking operations for reproducibility.
#'
#' @export
user_data <- R6::R6Class(
  "UserData",
  public = list(
    #' @field excel_data_trial Path to the current Excel file used for import/export operations.
    excel_data_trial = NULL,
    
    #' @field combined_data A data.frame containing all observation data merged and standardized.
    combined_data = NULL,
    
    #' @field obs_data A named list of data.frames, each corresponding to a sheet or file containing observation data.
    obs_data = list(),
    
    #' @field metadata A list storing metadata tables, such as "placette" and "modalite".
    metadata = list(),
    
    #' @field traceability A data.frame storing a log of all operations performed on the data (import, export, update, etc.).
    traceability = NULL,
    
    #' @description
    #' Initializes a new `user_data` object. If no Excel file is provided, a default template is used.
    #'
    #' @param trial_file Optional. Path to the Excel file to load. If NULL, a default template is used.
    #' 
    #' @return A new instance of the `UserData` class.
    initialize = function(trial_file = NULL) {
      # If no file is provided, use the default template
      if (is.null(trial_file)) {
        self$excel_data_trial <- system.file("extdata", "template.xlsx", package = "startbox")
        message("📄 No file provided. Using default template.")
      } else {
        self$excel_data_trial <- trial_file
        message("📄 Using provided Excel file: ", basename(trial_file))
      }
      
      self$traceability <- data.frame(
        datetime = character(),
        operation = character(),
        filename = character(),
        description = character(),
        package_version = character(),
        stringsAsFactors = FALSE
      )
    },
    
    #' @description
    #' Adds or updates a metadata element in the `metadata` slot.
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
    
    #' @description
    #' Adds a new observation dataset to `obs_data` or replaces an existing one.
    #' Automatically logs the operation and adds provenance columns.
    #'
    #' @param name Name of the observation source.
    #' @param df A data.frame containing the observation data.
    #' @param overwrite Logical. If TRUE, replaces an existing entry with the same name.
    #' 
    #' @return None. The object is modified in place.
    add_obs = function(name, df, source_file = NULL,  overwrite = FALSE) {
      
      filename_base <- if (!is.null(source_file)) source_file else name     
      
      if (name %in% names(self$obs_data)) {
        if (!overwrite) {
          message(paste("❌ Element already exists and will not be overwritten:", name))
          return(invisible(NULL))
        } else {
          message(paste("🔁 Updating existing element:", name))
          
          self$log_trace(
            operation = "update_data",
            filename = filename_base,
            description = "Observation updated via add_obs"
          )
        }
      } else {
        message(paste("✅ Adding a new element:", name))
        
        self$log_trace(
          operation = "import",
          filename = filename_base,
          description = "New observation added via add_obs"
        )
      }
      
      self$obs_data[[name]] <- df
    },
    
    #' @description
    #' Displays the first few rows of each observation dataset stored in `obs_data`.
    #'
    #' @return A list of data.frames (head of each dataset).
    show_obs_data = function() {
      lapply(self$obs_data, head)
    },
    
    
    #' @description
    #' This function adds a new entry to the `traceability` log stored in the R6 object.
    #' It records the type of operation, the target file or sheet name(s), and the timestamp.
    #'
    #' @param operation A character string describing the action performed (e.g. `"import"`, `"export"`, `"update"`).
    #' @param filename A character string indicating the name(s) of the file or sheet involved in the operation.
    #'
    #' @return No return value. This function updates the internal `traceability` data frame.
    log_trace = function(operation, filename, description = "") {
      package_version <- tryCatch({
        paste0("startbox v", as.character(utils::packageVersion("startbox")))
      }, error = function(e) {
        "startbox vUNKNOWN"
      })
      
      new_entry <- data.frame(
        datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        operation = operation,
        filename = filename,
        description = description,
        package_version = package_version,
        stringsAsFactors = FALSE
      )
      
      self$traceability <- dplyr::bind_rows(self$traceability, new_entry)
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
      load_metadata_sheets(self)
      
      # Merge with existing 'data' sheet if it exists
      wb <- openxlsx2::wb_load(self$excel_data_trial)
      combined <- merge_with_existing_data(wb, combined)
      
      self$combined_data <- combined
      return(combined)
    }
  )
)
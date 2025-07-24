#' @title User Data R6 Class
#'
#' @description
#' This R6 class is designed to manage experimental data, including observation sheets, metadata,
#' and traceability logs. It provides methods to import, merge, and export data from Excel files
#' while tracking operations for reproducibility.
#'
#' @importFrom R6 R6Class
#'
#' @export
user_data <- R6::R6Class(
  "UserData",
  public = list(
    #' @field name name of the user_data object, by default "user_data".
    name = "user_data",

    #' @field excel_data_trial Path to the current Excel file used for import/export operations.
    excel_data_trial = NULL,

    #' @field obs_data A named list of data.frames, each corresponding to a sheet or file containing observation data.
    obs_data = list(),

    #' @field metadata A list storing metadata tables, such as "placette" and "modalite".
    metadata = list(),

    #' @field plot_tnt_association A list storing association tables between plot and TNT.
    plot_tnt_association = list(
      user_association = NULL,
      block_association = NULL,
      nearest_association = NULL
    ),

    #' @field prepared_data A list storing prepared_data tables ready for stats analysis as produced by the function prepare_data()
    prepared_data = list(),

    #' @field stats A list storing stats results as results of the function stats_tests()
    stats = list(),

    #' @field traceability A data.frame storing a log of all operations performed on the data (import, export, update, etc.).
    traceability = NULL,

    #' @description
    #' Initializes a new `user_data` object. If no Excel file is provided, a default template is used.
    #'
    #' @param trial_file Optional. Path to the Excel file to load. If NULL, a default template is used.
    #' @param name optional. A string to name the new `user_data` object
    #'
    #' @return A new instance of the `UserData` class.
    initialize = function(trial_file = NULL, name = NULL) {
      # If no file is provided, use the default template
      if (is.null(trial_file)) {
        self$excel_data_trial <- system.file(
          "extdata",
          "template.xlsx",
          package = "startbox"
        )
        message("📄 No file provided. Using default template.")
      } else {
        self$excel_data_trial <- trial_file
        message("📄 Using provided Excel file: ", basename(trial_file))
        if (is.null(name)) {
          self$name <- sub("\\..*", "", basename(trial_file))
        } else {
          self$name <- name
        }
        load_data_sheets(self)
        load_metadata_sheets(self)
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
    #' @param source_file the source file path
    #'
    #' @return None. The object is modified in place.
    add_obs = function(name, df, source_file = NULL, overwrite = FALSE) {
      filename_base <- if (!is.null(source_file)) source_file else name

      if (name %in% names(self$obs_data)) {
        if (!overwrite) {
          message(paste(
            "❌ Element already exists and will not be overwritten:",
            name
          ))
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
    #' @param description A description of the operation to store in log
    #'
    #' @return No return value. This function updates the internal `traceability` data frame.
    log_trace = function(operation, filename, description = "") {
      package_version <- tryCatch(
        {
          paste0("startbox v", as.character(utils::packageVersion("startbox")))
        },
        error = function(e) {
          "startbox vUNKNOWN"
        }
      )

      new_entry <- data.frame(
        datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        operation = operation,
        filename = filename,
        description = description,
        package_version = package_version,
        stringsAsFactors = FALSE
      )

      self$traceability <- dplyr::bind_rows(self$traceability, new_entry)
    }
  )
)

#' @title Prepare Final Merged Dataset with Metadata
#'
#' @description
#' Merges observation data with metadata sheets ("placette" and "modalite") if available.
#' The function first joins the plot-level and treatment-level metadata, then links the result to the combined observation data
#' using the `plot_id` column.
#'
#' @param self An instance of the `UserData` R6 class containing observation data and metadata.
#'
#' @return A `data.frame` with observation data enriched by plot and treatment metadata.
#' If no observation data is available, returns only the merged plot + modality metadata.
#' If required metadata is missing, returns `NULL` with a message.
#'
#' @details
#' - Assumes that `self$metadata$plot_desc` contains `factor_level_code` and `plot_id`.
#' - Assumes that `self$metadata$moda_desc` contains `xp_trt_code`.
#' - If `self$combined_data` is not set, calls to `combine_data_obs()` are made automatically.
#'
#' @export
prepare_final_data <- function(self) {
  
  self$combine_data_obs()
  
  if (is.null(self$metadata$plot_desc) || is.null(self$metadata$moda_desc)) {
    message("plot_desc or moda_desc missing.")
    return(NULL)
  }
  
  # Step 1. Join plot and modality
  self$metadata$plot_desc$factor_level_code <- as.character(self$metadata$plot_desc$factor_level_code)
  self$metadata$moda_desc$xp_trt_code <- as.character(self$metadata$moda_desc$xp_trt_code)
  
  df_plot_moda <- dplyr::left_join(
    self$metadata$plot_desc,
    self$metadata$moda_desc,
    by = c("factor_level_code" = "xp_trt_code")
  )
  
  # Explicitly add xp_trt_code
  df_plot_moda$xp_trt_code <- df_plot_moda$factor_level_code
  
  # Step 2. Add observation data
  if (length(self$obs_data) == 0) {
    message("No observation data to join. Returning only plot + modality.")
    return(df_plot_moda)
  }
  
  if (is.null(self$combined_data)) {
    message("Combined data is not ready yet. Call combine_data_obs() first.")
    return(NULL)
  }
  
  df_obs <- self$combined_data
  df_obs$plot_id <- as.character(df_obs$plot_id)
  df_plot_moda$plot_id <- as.character(df_plot_moda$plot_id)
  
  df_obs$plot_id <- harmonize_plot_id_format(df_obs$plot_id)
  
  df_final <- dplyr::left_join(df_obs, df_plot_moda, by = "plot_id")
  
  return(df_final)
}


#' @title Combine and Reorder Observation Data
#'
#' @description
#' Combines all observation datasets into a single dataframe and reorders columns
#' to follow a standard structure.
#'
#' @param obs_data A named list of harmonized observation dataframes. Each element should contain at least some of the standard columns like `plot_id`, `bbch_stage`, etc.
#'
#' @return A single combined dataframe with standard columns (`prov_name`, `prov_date`, `observation_date`, `bbch_stage`, `plot_id`) appearing first, followed by all other columns in their original order.
#'
#' @details
#' - The function is typically used after harmonizing all observation datasets.
#' - Columns not in the template list are retained and placed after the standard ones.
#' - The final output is useful for exporting or merging with other trial data.
#'
#' @export
combine_and_reorder_obs <- function(obs_data) {
  combined <- dplyr::bind_rows(obs_data)

  template_cols <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
  valid_cols <- intersect(template_cols, names(combined))
  ordered_cols <- c(valid_cols, setdiff(names(combined), valid_cols))

  combined <- combined[, ordered_cols, drop = FALSE]
  return(combined)
}


#' @title Merge Combined Data with Existing Excel Sheet
#'
#' @description
#' Merges newly combined observation data with any existing data stored in the "data" sheet of an Excel workbook.
#' Ensures consistent data types and avoids duplication based on `prov_name`.
#'
#' @param wb A `workbook` object from the `openxlsx2` package, representing the loaded Excel trial file.
#' @param combined A `data.frame` containing newly combined and standardized observation data to be merged.
#'
#' @return A `data.frame` containing both the original and new data, with duplicates replaced if applicable.
#'
#' @details
#' - Converts key columns (`prov_name`, `plot_id`, etc.) to character to ensure compatibility.
#' - Converts all `*_PC` columns to numeric format.
#' - If the `prov_name` already exists in the original data, the corresponding rows are replaced by the new data.
#' - Removes the existing "data" worksheet from the workbook to prepare for writing the updated version.
#'
#' @export
merge_with_existing_data <- function(wb, combined) {
  if ("data" %in% openxlsx2::wb_get_sheet_names(wb)) {
    old_data <- openxlsx2::wb_read(wb, sheet = "data")

    # Harmonize sensitive columns as character
    columns_to_character <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
    for (col in columns_to_character) {
      if (col %in% names(old_data)) old_data[[col]] <- as.character(old_data[[col]])
      if (col %in% names(combined)) combined[[col]] <- as.character(combined[[col]])
    }

    # *_PC columns to numeric
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
        message(paste0("Updating data for: ", paste(prov_names_to_update, collapse = ", ")))
        old_data <- old_data[!(old_data$prov_name %in% prov_names_to_update), ]
      }
    }

    # Finally, concatenate
    combined <- dplyr::bind_rows(old_data, combined)

    # Remove the old sheet
    openxlsx2::wb_remove_worksheet(wb,"data")
  }

  return(combined)
}

#' Harmonize plot_id format to match placette (e.g. 10A to A10)
#'
#' @param pid vector of plot_id (character)
#'
#' @return harmonized plot_id (e.g. A10, TNT3, etc.)
#' @export
harmonize_plot_id_format <- function(pid) {
  pid <- trimws(pid)
  pid <- toupper(pid)

  is_tnt <- grepl("^TNT", pid)

  pid_clean <- ifelse(
    is_tnt,
    pid,
    gsub("^([0-9]{1,2})([A-Z])$", "\\2\\1", pid)
  )

  return(pid_clean)
}

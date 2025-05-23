#' @description
#' Merges observation data with metadata sheets ('placette' and 'modalite') if available.
#' The function first joins the plot and treatment metadata, then links this to the combined observation data
#' based on the 'plot_id' column.
#'
#' @return A data.frame with observation data joined with plot and treatment metadata.
#' If no observation data is present, returns only the joined metadata.
#' @export
prepare_final_data <- function(self) {
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


#' @description
#' Combines all observation datasets into a single dataframe and reorders columns.
#'
#' @param obs_data A list of harmonized observation datasets.
#'
#' @return A combined and reordered dataframe.
#' @export
combine_and_reorder_obs <- function(obs_data) {
  combined <- dplyr::bind_rows(obs_data)

  template_cols <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
  valid_cols <- intersect(template_cols, names(combined))
  ordered_cols <- c(valid_cols, setdiff(names(combined), valid_cols))

  combined <- combined[, ordered_cols, drop = FALSE]
  return(combined)
}


#' @description
#' Merges new combined data with existing data in the Excel file.
#'
#' @param wb The openxlsx2 workbook object.
#' @param combined The newly combined data to insert.
#'
#' @return A dataframe containing old data + new combined data.
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

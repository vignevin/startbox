## utils small functions in startbox

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Look star variables in a dataframe
#'
#' @param df a dataframe
#'
#' @returns a vector of variables found in the dataframe that are also in the dictionary
#' @export
#'
find_vars <- function(df) {

  dico_path = system.file(
    "extdata",
    "star_dictionary.csv",
    package = "startbox"
  )
  dico <- utils::read.csv2(dico_path, stringsAsFactors = FALSE)
  star_variables <- dico$nom[dico$is.variable]
  vars_found <- names(df)[sapply(
    names(df),
    function(t)
      any(sapply(star_variables, function(m) grepl(m, t, ignore.case = T)))
  )]
  if (length(vars_found) == 0) {
    message(paste("No variables found in the data frame"))
    return(NULL)
  }
  message("variables found:", paste(vars_found, collapse = ","))
  return(vars_found)
}

#' Ordering factors
#'
#' @param f a vector of type character or factor to be ordered
#'
#' @returns a vector of factors with the numeric one first
#' @export
#'
order_factor_levels_numeric_last <- function(f) {
  f <- as.factor(f)
  # Get unique levels
  levs <- levels(f)

  # Identify which levels are numeric
  is_num <- suppressWarnings(!is.na(as.numeric(levs)))

  # Separate numeric and non-numeric levels
  num_levels <- as.numeric(levs[is_num])
  char_levels <- levs[!is_num]

  # Order numeric levels numerically, keep character levels as is (or sort if preferred)
  ordered_num_levels <- as.character(sort(num_levels))

  # Combine numeric and non-numeric levels
  new_levels <- c(char_levels, ordered_num_levels)

  # Return  new levels
  return(new_levels)
}


#' Check differences in plot_id
#'
#' @description
#' Check differences between plot_id values between a reference dataframe (typically the plot_desc sheet) and an other dataframe (typically an observation dataset)
#'
#'
#'
#' @param df_as_ref a reference dataframe with a plot_id col
#' @param df_to_test a dataframe to test with a plot_id col
#'
#' @returns TRUE if differences are detected (NA are ignored)
#' @export
#'
check_plotid_diff <- function(df_as_ref, df_to_test) {
  ## check if plot_id if in both dataframes
  flag_diff = FALSE
  if (!"plot_id" %in% intersect(colnames(df_as_ref), colnames(df_to_test))) {
    message("col plot_id not in the 2 dataframes")
    return(NULL)
  }
  df_as_ref.plotid <- as.character(df_as_ref$plot_id)
  df_to_test.plotid <- as.character(df_to_test$plot_id)

  diff2 <- setdiff(df_to_test.plotid, df_as_ref.plotid)
  diff2 <- stats::na.omit(diff2)
  if (length(diff2) > 0) {
    message(paste(
      "plot_id:",
      paste(diff2, collapse = ","),
      "not in the reference plot description"
    ))
    flag_diff = TRUE
  }
  return(flag_diff)
}


#' Check differences in xp_trt_code
#'
#' @description
#' Check differences between xp_trt_code values between a reference dataframe (typically moda_desc)
#' and an other dataframe (typically an observation dataset).
#' Most of the time xp_trt_code are not saved in raw observation datasets :
#' observation dataset should then be merged before with plot_desc, using merge_data_plotdesc()
#'
#' @param df_as_ref a reference dataframe with a xp_trt_code col
#' @param df_to_test a dataframe to test with a xp_trt_code col
#'
#' @returns TRUE if differences are detected (NA are ignored)
#' @export
#'
check_xptrtcode_diff <- function(df_as_ref, df_to_test) {
  ## check if xp_trt_code if in both dataframes
  flag_diff = FALSE
  if (
    !"xp_trt_code" %in% intersect(colnames(df_as_ref), colnames(df_to_test))
  ) {
    message("col xp_trt_code not in the 2 dataframes")
    return(NULL)
  }
  df_as_ref.xptrtcode <- as.character(df_as_ref$xp_trt_code)
  df_to_test.xptrtcode <- as.character(df_to_test$xp_trt_code)

  diff2 <- setdiff(df_to_test.xptrtcode, df_as_ref.xptrtcode)
  diff2 <- stats::na.omit(diff2)
  if (length(diff2) > 0) {
    message(paste(
      "xp_trt_code:",
      paste(diff2, collapse = ","),
      "not in the reference experimental treatment description"
    ))
    flag_diff = TRUE
  }
  return(flag_diff)
}

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
    stop("Missing file", filepath)
  }

  if (tools::file_ext(filepath) != "csv") {
    stop("File must be a csv file")
  }

  df <- utils::read.csv2(file = filepath, fileEncoding = "latin1")

  colonnes_attendues <- c("Maladie", "Organe", "Valeur", "Placette", "Bloc")
  colonnes_manquantes <- setdiff(colonnes_attendues, names(df))

  if (length(colonnes_manquantes) > 0) {
    stop(
      "Not a Topvigne export file. Missing cols: ",
      paste(colonnes_manquantes, collapse = ", "),
      "."
    )
  }

  return(TRUE)
}

#' Find nearest tnt from plot
#' @description
#' This function calculate distances between plot and TNT (Non Treated Treatment) and find the nearest TNT for each plot.
#' @details
#' This function use the plot description, that must have plot_x and plot_y given for each plot.
#' Plot description is merged with modality description, assuming that xp_trt_name contains "TNT" to identify plot with TNT treatment.
#' The results is stored in self$plot_tnt_association$nearest_association
#'
#'
#' @param self an instance of the `UserData` R6 class containing metadata with plot_desc and moda_desc
#' @param scale_factor_x multiplier factor for calculating distances along the x axis (default = 2.5, assuming x axis for vines rows)
#' @param scale_factor_y multiplier factor for calculating distances along the y axis (default = 1)
#' @param code_tnt string to identify in TNT in the row of the dataframe, by default "TNT"
#'
#' @returns a dataframe with plot_id associated with one (or more) tnt_id, and their calculated distance
#' @export
#' @importFrom dplyr filter bind_rows
#'
nearest_tnt <- function(
  self,
  scale_factor_x = 2.5,
  scale_factor_y = 1,
  code_tnt = "TNT"
) {
  # local binding
  xp_trt_name <- plot_id <- NULL

  data <- self$metadata$plot_desc
  data <- merge(data, self$metadata$moda_desc)

  # Separate the data into two groups: TNT and others
  tnt_data <- data %>% dplyr::filter(xp_trt_name == code_tnt)
  non_tnt_data <- data %>% dplyr::filter(xp_trt_name != code_tnt)

  # Initialize a list to store the results
  results <- list()

  # Browse every non-TNT item
  for (i in 1:nrow(non_tnt_data)) {
    current_obj <- non_tnt_data[i, ]

    # Adjust TNT object coordinates with scale factors
    adjusted_tnt_x <- tnt_data$plot_x * scale_factor_x
    adjusted_tnt_y <- tnt_data$plot_y * scale_factor_y

    adjusted_current_x <- current_obj$plot_x * scale_factor_x
    adjusted_current_y <- current_obj$plot_y * scale_factor_y

    # Calculate distances with adjusted coordinates
    distances <- sqrt(
      ((adjusted_tnt_x - adjusted_current_x)^2 +
        (adjusted_tnt_y - adjusted_current_y)^2)
    )

    # Identify the TNT object with the minimum distance
    min_distance <- min(distances)
    nearest_TNT_id <- tnt_data$plot_id[distances == min_distance]

    # Store the results
    results[[i]] <- data.frame(
      plot_id = current_obj$plot_id,
      tnt_id = nearest_TNT_id,
      distance = min_distance
    )
  }

  # Combine the results into a dataframe
  result_df <- dplyr::bind_rows(results)

  ## add tnt
  tnt_data %>%
    dplyr::mutate(tnt_id = plot_id, distance = 0) %>%
    dplyr::select(names(result_df)) -> tnt_data

  result_df <- dplyr::bind_rows(result_df, tnt_data)

  self$plot_tnt_association$nearest_association <- result_df
}


#' Tnt by block
#' @description
#' This function associate each TNT (Non Treated Treatment) to one block.
#' @details
#' This function use the plot description, that must have plot_id and block_code given for each plot.
#' Plot description is merged with modality description, assuming that each row that contains the string given by code_TNT ("TNT")
#' is identified as a plot with TNT treatment.
#' The results is stored in self$plot_tnt_association$block_association
#' @param self an instance of the `UserData` R6 class containing metadata with plot_desc and moda_desc
#' @param code_tnt string to identify in TNT in the row of the dataframe, by default "TNT"#'
#' @returns a dataframe with plot_id associated with one (or more) tnt_id in the same block
#' @export
#'
block_tnt <- function(self, code_tnt = "TNT") {

  # local binding
  block_code <- plot_id <- . <- NULL

  data <- self$metadata$plot_desc
  data <- merge(data, self$metadata$moda_desc)
  if (!"plot_id" %in% names(data)) {
    message("plot_id not found; function aborted")
    return(NULL)
  }
  if (!"block_code" %in% names(data)) {
    message("block_code not found; function aborted")
    return(NULL)
  }

  data %>%
    dplyr::filter(apply(
      .,
      1,
      function(row) any(grepl(code_tnt, row))
    )) %>%
    dplyr::select(plot_id, block_code) %>%
    dplyr::rename(tnt_id = plot_id) %>%
    unique() -> tnt_df

  result_df <- merge(data, tnt_df, by.x = "block_code", by.y = "block_code")
  result_df %>%
    dplyr::select("tnt_id", "plot_id", "block_code") -> result_df

  self$plot_tnt_association$block_association <- result_df
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

  # Extract the numeric and alphabetic parts
  num_part <- gsub("[^0-9]", "", pid)
  char_part <- gsub("[^A-Za-z]", "", pid)

  # new pid
  pid_clean <- paste0(char_part, num_part)

  # is_tnt <- grepl("^TNT", pid)
  #
  # pid_clean <- ifelse(
  #   is_tnt,
  #   pid,
  #   gsub("^([0-9]{1,2})([A-Z])$", "\\2\\1", pid)
  # )

  return(pid_clean)
}


#' Names of data observations available in an user_data objetc
#'
#' @param self #' @param self an instance of the `UserData` R6 class
#'
#' @returns a vector of names of data_observations
#' @export
#'
obsnames <- function(self) {
  names(self$obs_data)
}

#' Names of data observations available in an user_data objetc
#'
#' @param self #' @param self an instance of the `UserData` R6 class
#'
#' @returns a vector of names of prepared data
#' @export
#'
prpnames <- function(self) {
  names(self$prepared_data)
}


#' Names of stats available in an user_data objetc
#'
#' @param self self an instance of the `UserData` R6 class
#'
#' @returns a vector of names of prepared data
#' @export
#'
statsnames <- function(self) {
  names(self$stats)
}

#' Extract dataframe from an user_data object by his name (looking in obs_data and prepared_data)
#'
#' @param self an instance of the `UserData` R6 class containing user data.
#' @param dfname a name to found (character)
#'
#' @returns a dataframe if found, NULL if not
#' @export
#'
getdataframe <- function(self = NULL,dfname = NULL) {
  params <- list(self,dfname)
  if(any(sapply(params, is.null))) {
    message("missing parameter(s)")
    return(NULL)
    }
  if(dfname %in% obsnames(self)) {
    df <- self$obs_data[[dfname]]
    return(df)
  }
  if(dfname %in% prpnames(self)) {
    df <- self$prepared_data[[dfname]]
    return(df)
  }
  message(dfname," not found nor in either obs_data or prepared_data")
}

#' @title Check if weather data are daily or hourly
#'
#' @description
#' This function analyzes the `meteo_datetime` column from the `weather` slot
#' of the `user_data` object and determines if the data are daily or hourly observations.  
#' It is mainly used by [plot_meteo()] to ensure that daily data are plotted correctly.
#'
#' @param self A `user_data` R6 object containing a `weather` data.frame with a column named `meteo_datetime`.
#'
#'
#' @return
#' A logical value:
#' `TRUE` — if the dataset represents daily weather data.  
#' `FALSE` — if the dataset contains hourly (or sub-daily) timestamps.
#'
#' @importFrom base as.POSIXct as.Date format suppressWarnings
#' 
#' @export
check_daily_meteo <- function(self) {
  
  if (is.null(self$weather) || nrow(self$weather) == 0) {
    stop("[check_daily_meteo] self$weather est vide. Charge d'abord via load_weather_sheets().", call. = FALSE)
  }
  if (!("meteo_datetime" %in% names(self$weather))) {
    stop("[check_daily_meteo] Colonne 'meteo_datetime' absente de self$weather.", call. = FALSE)
  }
  
  x <- self$weather$meteo_datetime
  
  to_posix <- function(v) {
    if (inherits(v, "POSIXct")) return(v)
    if (inherits(v, "Date"))    return(as.POSIXct(v, tz = "UTC"))
    fmts <- c(
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
      "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
      "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M",
      "%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y"
    )
    parsed <- rep(NA_real_, length(v))
    parsed <- as.POSIXct(parsed, origin = "1970-01-01", tz = "UTC") 
    for (f in fmts) {
      tmp <- suppressWarnings(as.POSIXct(v, format = f, tz = "UTC"))
      need <- is.na(parsed) & !is.na(tmp)
      parsed[need] <- tmp[need]
      if (!any(is.na(parsed))) break
    }
    parsed
  }
  
  dt <- to_posix(x)
  if (all(is.na(dt))) {
    stop("[check_daily_meteo] Impossible de parser 'meteo_datetime' en date-heure.", call. = FALSE)
  }
  
  dt <- dt[!is.na(dt)]
  if (length(dt) == 0) {
    stop("[check_daily_meteo] Toutes les valeurs de 'meteo_datetime' sont NA après parsing.", call. = FALSE)
  }
  
  jours <- as.Date(dt, tz = "UTC")
  counts <- table(jours)
  has_multiple_per_day <- any(as.integer(counts) > 1L)
  
  hh <- as.integer(format(dt, "%H"))
  mm <- as.integer(format(dt, "%M"))
  ss <- as.integer(format(dt, "%S"))
  has_non_midnight_time <- any((hh != 0L | mm != 0L | ss != 0L) & !is.na(hh))
  
  is_daily <- !(has_multiple_per_day || has_non_midnight_time)
  return(is_daily)
}

check_pom <- function(df, required = c("DATE", "PLUIE", "TMOY")) {
  # Vérifie que df est bien un data.frame
  if (!is.data.frame(df)) {
    stop("[check_pom] L'objet fourni n'est pas un data.frame.", call. = FALSE)
  }
  
  # Colonnes réellement présentes
  cols <- names(df)
  
  # Colonnes manquantes
  missing <- setdiff(required, cols)
  
  # Si des colonnes manquent → message d'erreur clair
  if (length(missing) > 0) {
    stop(
      paste0(
        "[check_pom] Le fichier ne contient pas toutes les colonnes requises :\n",
        "Colonnes manquantes : ", paste(missing, collapse = ", "), "\n",
        "Colonnes trouvées : ", paste(cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Si tout va bien
  message("[check_pom] ✅ Toutes les colonnes nécessaires sont présentes : ",
          paste(required, collapse = ", "))
  return(TRUE)
}


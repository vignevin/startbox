#' Resume data
#'
#' @description
#' This function summarizes the data in a dataframe according to user-defined groups and functions. In particular, it allows you to calculate the intensity and frequency of disease attack per plot.
#'
#' @param data a dataframe to resume
#' @param var_col character, the colname of the variable to plot
#' @param group_cols the colname of the group
#' @param funs statistic to plot, a vector of one or two statistic. by default c("mean","frequency")
#' @param code_tnt a string to identify in TNT in the row of the dataframe by default "TNT"
#' @param df_tnt a dataframe to associate tnt_id to bloc_id or plot_id. tnt_id is the plot_id of TNT and MUST be a col of df_tnt
#'
#' @returns a dataframe ready for plotting
#' @export
resume_data <- function(
  data,
  var_col,
  group_cols,
  funs = list(intensite = mean, frequence = frequency),
  code_tnt = "TNT",
  df_tnt = NULL
) {
  # flag_variable
  flag_variable <- "variable" %in% colnames(data)

  # convert column argument to symbol
  var <- dplyr::ensym(var_col)
  if (flag_variable) group_syms <- dplyr::syms(c(group_cols, "variable")) else
    group_syms <- dplyr::syms(group_cols)
  if (flag_variable) group_tnt <- dplyr::syms("calculation") else group_tnt <- NULL

  # if df_tnt is not null, calculation by block_id given in df_tnt
  if (!is.null(df_tnt)) {
    # check if df_tnt is a dataframe
    stopifnot(is.data.frame(df_tnt))
    # check if tnt_id is a col in  'df_tnt'
    if (!("tnt_id" %in% colnames(df_tnt))) {
      stop("'tnt_id' must be a col of 'df_tnt'.")
    }
    group_tnt <- c(group_tnt, dplyr::syms("plot_id"))
  }

  data_resume <- data.frame()
  for (i in 1:length(funs)) {
    if (identical(funs[[i]], startbox::efficacy)) {
      ## calcul of tnt mean
      data %>%
        dplyr::filter(apply(., 1, function(row) any(grepl(code_tnt, row)))) %>%
        dplyr::group_by(!!!group_tnt) %>%
        dplyr::summarise(
          mean_tnt = mean({{ var }}, na.rm = T),
          .groups = "drop"
        ) -> mean_tnt

      if (is.na(mean_tnt[1, 1]) | nrow(mean_tnt) == 0) {
        print("no TNT value(s) found in data, please check your data")
        stop()
      }

      if ("plot_id" %in% colnames(mean_tnt)) {
        mean_tnt %>%
          dplyr::rename(tnt_id = plot_id) -> mean_tnt
      }

      ## join with data
      if (!is.null(df_tnt)) {
        merge(df_tnt, mean_tnt) -> mean_tnt
      }

      if (!is.null(group_tnt)) {
        data <- merge(data, mean_tnt)
      } else {
        data$mean_tnt = as.numeric(mean_tnt)
      }

      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(
          mean_tnt = mean(mean_tnt),
          sd = sd({{ var }}, na.rm = TRUE),
          value = startbox::efficacy({{ var }}, value_tnt = mean_tnt),
          nb = n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          calculation = if ("calculation" %in% colnames(data))
            paste(calculation, !!names(funs)[i]) else !!names(funs)[i]
        )
    } else {
      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(
          sd = sd({{ var }}, na.rm = TRUE),
          value = funs[[i]]({{ var }}),
          nb = n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          calculation = if ("calculation" %in% colnames(data))
            paste(calculation, !!names(funs)[i]) else !!names(funs)[i]
        )
    }
    data_resume <- dplyr::bind_rows(data_resume, resume)
    if (!flag_variable) data_resume$variable = var_col
  }
  return(data_resume)
} #end function

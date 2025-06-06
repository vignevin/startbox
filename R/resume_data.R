#' Resume data
#'
#' @description
#' This function summarizes the data in a dataframe according to user-defined groups and functions. In particular, it allows you to calculate the intensity and frequency of disease attack per plot.
#'
#' @param data a dataframe to resume
#' @param var_col character, the colname of the variable to plot
#' @param group_cols colnames for grouping
#' @param funs vector of statistics to be applied, by default c("intensity","incidence")
#' @param code_tnt for efficacy only : a string to identify in TNT in the row of the dataframe by default "TNT"
#' @param df_tnt for efficacy only : a dataframe to associate tnt_id to bloc_id or plot_id. tnt_id is the plot_id of TNT and MUST be a col of df_tnt
#'
#' @returns a dataframe with group_cols and including a 'calculation' column which specifies the name of the functions applied to the variable and a 'value' column which gives the calculated values.
#' @export
#' @importFrom dplyr ensym syms filter group_by summarise n rename bind_rows

resume_data <- function(
  data,
  var_col,
  group_cols,
  funs = list(intensite = intensity, frequence = incidence),
  code_tnt = "TNT",
  df_tnt = NULL
) {

  # local binding
  calculation <- plot_id <- . <- NULL

  # convert column argument to symbol
  var <- dplyr::ensym(var_col)
  group_syms <- dplyr::syms(group_cols)
  group_tnt <- NULL

  # # if calculation is in colnames (means that this is already a df from a previous iteration of resume_data)
  if ("calculation" %in% colnames(data)) {
      group_tnt <- dplyr::syms("calculation") #
      group_syms <- dplyr::syms(c(group_cols, "calculation"))
  }

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
          nb_tnt = dplyr::n(),
          .groups = "drop"
        ) -> mean_tnt

      if (is.na(mean_tnt[1, 1]) | nrow(mean_tnt) == 0) {
        warning("⚠️ No TNT value(s) found for efficacy calculation. Skipping this iteration.")
        next
      } else {print(paste(unique(mean_tnt$nb_tnt),code_tnt,"used for calculation of efficacy"))}

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
        data$mean_tnt = as.numeric(mean_tnt$mean_tnt)
      }

      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(
          mean_tnt = mean(mean_tnt),
          value = startbox::efficacy({{ var }}, value_tnt = mean_tnt),
          nb = dplyr::n(),
          .groups = "drop"
        )
    } else {
      resume <- data %>%
        dplyr::group_by(!!!group_syms) %>%
        dplyr::summarise(
          value = funs[[i]]({{ var }}),
          nb = dplyr::n(),
          .groups = "drop"
        )
    }
    ## to add calculation
    default_name <- ifelse(var_col != "value",paste(names(funs)[i], var_col),names(funs)[i])
    resume %>%
      dplyr::mutate(
        calculation = if("calculation" %in% colnames(.)) paste(default_name, calculation) else default_name
        ) -> resume
    ## to add to resume data
    data_resume <- dplyr::bind_rows(data_resume, resume)

  }
  return(data_resume)
} 




#' Pivot wider a resume dataframe
#'
#' @param df_resume a dataframe returned by resume_data function
#'
#' @returns a dataframe in wider format
#' @export
#' @importFrom tidyr pivot_wider
#'
#' @examples
resume_pivot_wider <- function(df_resume)
{
  # local binding
  calculation <- value <- NULL

  if (!is.data.frame(df_resume)) {
    stop("Input must be a data frame")
  }

  if ("calculation" %in% colnames(df_resume)  && "value" %in% colnames(df_resume))
  {
  df_resume %>%
    tidyr::pivot_wider(names_from = calculation,values_from = value) -> df_resume
  }
  else {
    stop("Your dataframe MUST include cols 'calculation' and 'value'")
  }
  return(df_resume)
}


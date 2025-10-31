### graphs functions of startbox

## xpheat

#' @title Plot a Heatmap of Experimental Data
#'
#' @description
#' Generates a heatmap based on experimental observation data,
#' using `plot_x` and `plot_y` coordinates and coloring according to a selected disease variable (_PC columns).
#'
#' @param self An instance of the `UserData` R6 class
#' @param stats character, the name of a stats list inside self$stats.
#' @param calculation_choices vector of character, choice(s) of calculation to plot. For example c("FA UN_BER_PC","IA UN_BER_PC")
#' @param max_y (optional) Numeric. Maximum value for the fill color scale. If NULL, an automatic scale is calculated.
#' @param flip_orientation TRUE or FALSE to control plot orientation. Default is FALSE. see ?ggplot2::coord_flip()
#' @param yx_ratio specified ratio between the physical representation of data units on the axes. see ?ggplot2::coord_fixed()
#' @param resids Logical. If TRUE, residuals are calculated from the linear model and plotted
#' @param midpoint numeric, value of midpoint for fill_scale. Automatically set to 0 if resids = T or more than one calculation
#' @param short_names to have shorter names for fill removing the last part of the string after the last space occurence
#' @param ... other parameters for labs (title, x, y,fill, caption). see ?ggplot2::labs()
#'
#' @return A ggplot2 heatmap object.
#'
#' @details
#' - Aggregates values by `plot_id`, taking the mean when multiple observations exist.
#' - Useful for visualizing spatial patterns of disease in experimental trials.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot_xpheat <- function(
  self,
  stats = NULL,
  calculation_choices = NULL,
  max_y = NULL,
  flip_orientation = FALSE,
  yx_ratio = 0.3,
  resids = FALSE,
  midpoint = NULL,
  short_names = FALSE,
  ...
) {
  # local binding
  calculation <- value <- Valeurs <- value_centered <- plot_id <- plot_x <- plot_y <- NULL

  # check if self is UserData
  if (!inherits(self, "UserData")) {
    message(
      "Argument self is not an UserData object as required by the function"
    )
    return(NULL)
  }

  # check if stats exists
  if (!stats %in% names(self$stats)) {
    message(paste("no", stats, "found in stats. Function aborted"))
    return(NULL)
  }

  ## import stats in the function env
  stats <- self$stats[[stats]]
  data2plot <- self$prepared_data[[stats$prep_data]]

  ## if stats was not calculated on a calculation
  data2plot %>%
    dplyr::filter(
      calculation %in% unique(stats$df.grp_means$calculation)
    ) -> data2plot

  # Check columns
  required_cols <- c("plot_id", "plot_x", "plot_y", "calculation")

  missing <- setdiff(required_cols, names(data2plot))
  if (length(missing) > 0) {
    stop(paste("Missing required column(s):", paste(missing, collapse = ", ")))
  }

  ## if choice calculation not null
  calculation_levels <- unique(data2plot$calculation)
  if (!is.null(calculation_choices)) {
    if (length(intersect(calculation_choices, calculation_levels)) == 0) {
      message(paste(
        "One or more ",
        calculation_choices,
        "not found in stats data. Function aborted"
      ))
      return(NULL)
    } else {
      data2plot <- data2plot[data2plot$calculation %in% calculation_choices, ]
      calculation_levels <- unique(data2plot$calculation)
    }
  }

  if (resids) {
    for (calc in calculation_levels) {
      model <- stats::lm(
        as.formula(stats$df.stats$formula_used[
          stats$df.stats$calculation == calc
        ]),
        data = startbox::resume_pivot_wider(data2plot[
          data2plot$calculation == calc,
        ])
      )
      data2plot$residuals[data2plot$calculation == calc] <- model$residuals
    }
  }

  # Cleaning plot details
  data2plot <- dplyr::mutate(
    data2plot,
    plot_x = as.numeric(plot_x),
    plot_y = as.numeric(plot_y)
  ) %>%
    dplyr::filter(!is.na(plot_x) & !is.na(plot_y))

  # grouping col
  #grp_cols <- required_cols[-which(required_cols == "calculation")]
  grp_cols <- required_cols

  # Base summary
  summary_expr <- rlang::exprs(
    Valeurs = mean(value, na.rm = TRUE)
  )

  # Add residuals only if the column exists
  if ("residuals" %in% colnames(data2plot)) {
    summary_expr$Residuals <- rlang::expr(mean(residuals, na.rm = TRUE))
  }

  # Apply summarise
  data_agg <- data2plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
    dplyr::summarise(!!!summary_expr, .groups = "drop") %>%
    dplyr::group_by(calculation) %>%
    dplyr::mutate(
      value_centered = Valeurs - mean(Valeurs, na.rm = TRUE),
      value_centered_reduced = value_centered / stats::sd(Valeurs, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  if (nrow(data_agg) == 0) {
    stop("No aggregated data found.")
  }

  # Automatic scale
  if (is.null(max_y)) {
    max_abs <- max(abs(data_agg$Valeurs), na.rm = TRUE)
    max_y <- ceiling(max_abs * 1.1)
  }

  aes_x <- rlang::sym("plot_x")
  aes_y <- rlang::sym("plot_y")

  ## fill col
  fill_col <- rlang::sym("Valeurs") ## by default
  if (length(calculation_levels) > 1)
    fill_col <- rlang::sym("value_centered_reduced") ## for ploting more than one map values are centered-reduced
  if (resids) fill_col <- rlang::sym("Residuals")

  ## custom label
  short_name_calc <- calculation_levels
  if (short_names) {
    if (
      length(unique(gsub(" [^ ]*$", "", calculation_levels))) ==
        length(unique(calculation_levels))
    ) {
      short_name_calc <- gsub(" [^ ]*$", "", calculation_levels)
    } else {
    }
  }
  names(short_name_calc) = calculation_levels
  #p <- p + ggplot2::labs(fill = short_name_var)

  # midpoint for scale
  if (is.null(midpoint)) {
    if (resids | length(calculation_levels) > 1) {
      midpoint <- 0
    } else {
      midpoint <- mean(data_agg$Valeurs, na.rm = T)
    }
  }

  # Heatmap
  p <- ggplot2::ggplot(
    data_agg,
    ggplot2::aes(x = !!aes_x, y = !!aes_y, fill = !!fill_col)
  ) +
    ggplot2::facet_wrap(
      ~calculation,
      labeller = ggplot2::labeller(calculation = short_name_calc)
    ) +
    ggplot2::geom_tile(color = "grey", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = plot_id),
      color = "black",
      size = 4
    ) +
    ggplot2::scale_fill_gradient2(
      low = "green3",
      mid = "white",
      high = "red",
      midpoint = midpoint
    )

  #    axis_breaks +
  p <- p +
    ggplot2::coord_fixed(ratio = yx_ratio) + # ratio axes y/x
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey", fill = NA)
    )

  # to flip coords if asked
  if (flip_orientation) {
    p <- p + ggplot2::coord_flip()
  }

  # to add others parameters pass to labs (title, caption, etc.)
  p <- p + ggplot2::labs(...)
  return(p)
}


#' Barplot summary for an experiment
#'
#' @description
#' This function automatically generates a bar graph from observation or experimental data.
#'
#' @param self An instance of the `UserData` R6 class
#' @param stats character, the name of a stats list inside self$stats.
#' @param calculation_choices vector of character, choice(s) of calculation to plot. For example c("FA UN_BER_PC","IA UN_BER_PC")
#' @param one_plot boolean. If TRUE, all calculations are plot on an unique plot using fill argument, else each calculation is on a independent plot using factet
#' @param text_stat boolean. If TRUE, an annotation is added to the plot with the test done and its p-value
#' @param max_y Upper limit of the y axis. If NULL, automatic adjustment.
#' @param bar_color Bar fill colour.
#' @param bar_width bar width
#' @param border_tnt if true, the border color is set as red for TNT
#' @param show_tnt if false, TNT are removed from the graph
#' @param code_tnt a string to identify in TNT in the row of the dataframe by default "TNT"
#' @param show_errorbar display of error bars
#' @param show_data display data points
#' @param short_names to have shorter names for fill removing the last part of the string after the last space occurence
#' @param ... other parameters for labs (title, x, y,fill)
#'
#' @return a barplot
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot_xpbar <- function(
  self,
  stats = NULL,
  calculation_choices = NULL,
  one_plot = TRUE,
  text_stat = TRUE,
  max_y = NULL,
  bar_color = NULL,
  bar_width = 0.7,
  border_tnt = FALSE,
  show_tnt = TRUE,
  code_tnt = "TNT",
  show_errorbar = TRUE,
  show_data = TRUE,
  short_names = TRUE,
  ...
) {
  # local binding
  calculation <- . <- factor_level <- text <- NULL

  # check if self is UserData
  if (!inherits(self, "UserData")) {
    message(
      "Argument self is not an UserData object as required by the function"
    )
    return(NULL)
  }

  # check if stats exists
  if (is.null(self$prepared_data[[stats]])) {
    message(paste("no", stats, "found in stats. Function aborted"))
    return(NULL)
  }

  ## import stats in the function env
  stats <- self$stats[[stats]]
  data2plot <- stats$df.grp_means
  # original data

  data_points <- self$prepared_data[[stats$prep_data]]

  ## if stats was not calculated on a calculation
  data_points %>%
    dplyr::filter(calculation %in% unique(data2plot$calculation)) -> data_points

  col_x <- which(colnames(data_points) == unique(data2plot$factor))
  data_points[, col_x] <- as.factor(data_points[, col_x])
  colnames(data_points)[col_x] <- "factor_level"
  colnames(data_points)[which(colnames(data_points) == "value")] <- "mean"

  ## remonve tnt if show_tnt = false
  if (!show_tnt) {
    data2plot %>%
      dplyr::filter(
        !apply(
          .,
          1,
          function(row) any(grepl(code_tnt, row))
        )
      ) -> data2plot
    data_points %>%
      dplyr::filter(
        !apply(
          .,
          1,
          function(row) any(grepl(code_tnt, row))
        )
      ) -> data_points
  }

  ## test value
  ann_text <- stats$df.stats

  ## if choice calculation not null
  if (!is.null(calculation_choices)) {
    fill_levels <- unique(data2plot$calculation)
    if (length(intersect(calculation_choices, fill_levels)) == 0) {
      message(paste(
        "One or more ",
        calculation_choices,
        "not found in stats data. Function aborted"
      ))
      return(NULL)
    } else {
      data2plot <- data2plot[data2plot$calculation %in% calculation_choices, ]
      data_points <- data_points[
        data_points$calculation %in% calculation_choices,
      ]
      ann_text <- ann_text[ann_text$calculation %in% calculation_choices, ]
    }
  }

  # smart palette
  # n_colors
  fill_levels <- unique(data2plot$calculation)

  if (is.null(bar_color)) {
    if (any(grepl("LEAF", fill_levels, ignore.case = TRUE))) {
      bar_color <- grDevices::colorRampPalette(c("#d9f0d3", "#1b7837"))(length(
        fill_levels
      ))
    } else {
      bar_color <- grDevices::colorRampPalette(c("#f2e5ff", "#9966CC"))(length(
        fill_levels
      ))
    }
  }

  # Calculation of scale y
  # if (is.null(max_y)) {
  #   max_val <- max(data2plot[, ycol], na.rm = TRUE)
  #   max_y <- ceiling(max_val * 1.1)
  # }

  data2plot$factor_level <- factor(
    data2plot$factor_level,
    levels = order_factor_levels_numeric_last(data2plot$factor_level)
  )

  # if short_names = T
  if (short_names) {
    if (
      length(unique(data2plot$calculation)) ==
        length(unique(gsub(" [^ ]*$", "", data2plot$calculation)))
    ) {
      data2plot$calculation <- gsub(" [^ ]*$", "", data2plot$calculation)
      data_points$calculation <- gsub(" [^ ]*$", "", data_points$calculation)
      ann_text$calculation <- gsub(" [^ ]*$", "", ann_text$calculation)
    } else {
    }
  }

  # Create the graph
  p <- ggplot2::ggplot(
    data2plot,
    ggplot2::aes(x = factor_level, y = mean, fill = calculation)
  ) +
    ggplot2::geom_bar(
      ggplot2::aes(color = grepl("TNT", factor_level)), # logical test in aes
      stat = "summary",
      fun = "mean",
      position = ggplot2::position_dodge(width = bar_width),
      width = bar_width,
      linewidth = 0.5
    )

  if (border_tnt) {
    p <- p +
      ggplot2::scale_color_manual(
        values = c("FALSE" = "grey30", "TRUE" = "red")
      )
  } else {
    p <- p +
      ggplot2::scale_color_manual(
        values = c("FALSE" = "grey30", "TRUE" = "grey30")
      )
  }

  # if show_data
  if (show_data) {
    p <- p +
      ggplot2::geom_jitter(
        data = data_points,
        position = ggplot2::position_jitterdodge(
          jitter.width = 0.1,
          dodge.width = bar_width
        ),
        size = 2,
        alpha = 0.2
      )
  }

  # Conditional addition of error bars
  if (show_errorbar) {
    p <- p +
      ggplot2::stat_summary(
        data = data_points,
        fun.data = ggplot2::mean_se,
        geom = "errorbar",
        width = 0.2,
        position = ggplot2::position_dodge(width = bar_width)
      )
  }

  # Add text values
  p <- p +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(mean, 1), groups)),
      position = ggplot2::position_dodge(width = bar_width),
      fontface = "bold",
      vjust = -0.5,
      hjust = -0.5,
      size = 3
    )

  ## text stat
  if (text_stat) {
    ann_text$text <- paste(
      ann_text$calculation,
      ann_text$test,
      "p-value =",
      paste0(
        format(ann_text$ptest, scientific = TRUE, digits = 2),
        ann_text$signif
      )
    )
    if (one_plot) {
      ann_text$mean <- max(data_points$mean, na.rm = T) *
        (1 - 0:(nrow(ann_text) - 1) / 20) *
        1.1
    } else {
      ann_text$mean <- Inf
    }
    #ann_text$factor_level <- levels(data2plot$factor_level)[1]
    p <- p +
      ggplot2::geom_text(
        data = ann_text,
        fontface = "italic",
        ggplot2::aes(label = text, x = -Inf, hjust = -0.1, vjust = +1)
      )
  }

  # finalization
  p <- p +
    ggplot2::labs(..., fill = "", x = "") +
    ggplot2::scale_fill_manual(values = bar_color, drop = FALSE) +
    ggplot2::guides(color = "none") +
    #ggplot2::scale_color_identity() + ## TO CHECK UTILITY... OVERWRITE COLOR BAR RED TNT
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = if (length(fill_levels) > 1) "top" else "none"
    )

  if (!is.null(max_y)) {
    p <- p + ggplot2::ylim(0, max_y)
  }

  ## if facet on
  if (!one_plot) {
    p <- p +
      ggplot2::facet_wrap(~calculation, scales = "free_y") +
      ggplot2::guides(fill = "none")
  }

  return(p)
}


#' @title Plot ombrothermic weather chart
#'
#' @description
#' This function generates an ombrothermic plot from weather data stored in the `self$weather` attribute of the `user_data` object.  
#' It displays daily rainfall (in mm) as bars and daily minimum, mean, and maximum temperatures (in °C) as lines.
#'
#' The x-axis shows the months (horizontal labels) and the specific dates where
#' rainfall exceeded 5 mm (vertical labels aligned below the axis).
#'
#' @param self A `user_data` R6 object containing a `weather` data.frame
#' @param date_debut Optional start date for filtering the data (format `YYYY-MM-DD` or `DD/MM/YYYY`).
#' @param date_fin Optional end date for filtering the data.
#' @param afficher_dates Logical, default `TRUE`. If `TRUE`, adds date labels above rainfall bars where `rain_mm > 5`.
#' @param afficher_traitements Logical, default `TRUE`. If `TRUE`, draws treatment markers using unique dates found in `self$metadata$ppp$p_app_date` within the filtered range.
#' @param afficher_observations Logical, default `TRUE`. If `TRUE`, draws observation markers using unique `observation_date` values merged from all data frames in `self$obs_data` within the filtered range.
#'
#'
#' @return A `ggplot` object representing the ombrothermic chart.  
#' The used data (after cleaning and filtering) is attached as an attribute `"data_used"`.
#'
#' @export
plot_meteo <- function(self, date_debut = NULL, date_fin = NULL, afficher_dates = TRUE, afficher_traitements = TRUE, afficher_observations = TRUE) {
  
  if (is.null(self$weather) || nrow(self$weather) == 0) {
    stop("⚠️ Aucune donnée météo trouvée dans self$weather.", call. = FALSE)
  }
  
  if (!check_daily_meteo(self)) {
    stop("⏱️ Données horaires détectées dans 'meteo_datetime'.", call. = FALSE)
  }
  
  df <- self$weather
  names(df) <- tolower(names(df))
  
  attendues <- c("meteo_datetime", "air_tmin_celsius", "air_tmean_celsius", "air_tmax_celsius", "rain_mm")
  manquantes <- setdiff(attendues, names(df))
  if (length(manquantes) > 0) {
    stop(sprintf("Colonne(s) manquante(s) : %s", paste(manquantes, collapse = ", ")), call. = FALSE)
  }
  
  dt_formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
    "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
    "%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y"
  )
  to_posix <- function(x) {
    if (inherits(x, "POSIXct")) return(x)
    if (inherits(x, "Date"))    return(as.POSIXct(x, tz = "UTC"))
    out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
    for (f in dt_formats) {
      tmp  <- suppressWarnings(as.POSIXct(x, format = f, tz = "UTC"))
      need <- is.na(out) & !is.na(tmp)
      out[need] <- tmp[need]
      if (!any(is.na(out))) break
    }
    out
  }
  dt <- to_posix(df$meteo_datetime)
  if (any(is.na(dt))) stop("Impossible de convertir 'meteo_datetime' en date/heure.", call. = FALSE)
  df$meteo_datetime <- as.Date(dt)
  
  to_num <- function(x) {
    x <- gsub(",", ".", x, fixed = TRUE)
    x <- gsub("\\s+", "", x)
    suppressWarnings(as.numeric(x))
  }
  df$rain_mm           <- to_num(df$rain_mm)
  df$air_tmin_celsius  <- to_num(df$air_tmin_celsius)
  df$air_tmean_celsius <- to_num(df$air_tmean_celsius)
  df$air_tmax_celsius  <- to_num(df$air_tmax_celsius)
  
  if (all(is.na(df$air_tmean_celsius)) && any(!is.na(df$air_tmin_celsius) & !is.na(df$air_tmax_celsius))) {
    df$air_tmean_celsius <- (df$air_tmin_celsius + df$air_tmax_celsius) / 2
  }
  
  if (!is.null(date_debut)) date_debut <- as.Date(date_debut)
  if (!is.null(date_fin))   date_fin   <- as.Date(date_fin)
  if (!is.null(date_debut)) df <- dplyr::filter(df, meteo_datetime >= date_debut)
  if (!is.null(date_fin))   df <- dplyr::filter(df, meteo_datetime <= date_fin)
  if (nrow(df) == 0) stop("Aucune donnée dans la plage de dates sélectionnée.", call. = FALSE)
  
  df$rain_mm[df$rain_mm < 0] <- 0
  df$air_tmin_celsius[df$air_tmin_celsius < -50 | df$air_tmin_celsius > 70]    <- NA_real_
  df$air_tmean_celsius[df$air_tmean_celsius < -50 | df$air_tmean_celsius > 70] <- NA_real_
  df$air_tmax_celsius[df$air_tmax_celsius < -50 | df$air_tmax_celsius > 70]    <- NA_real_
  
  mm_max <- max(df$rain_mm, na.rm = TRUE)
  breaks_mm_droit <- seq(0, max(10, ceiling(mm_max / 10) * 10), by = 10)
  
  min_d <- min(df$meteo_datetime, na.rm = TRUE)
  max_d <- max(df$meteo_datetime, na.rm = TRUE)
  month_breaks <- seq(as.Date(format(min_d, "%Y-%m-01")),
                      as.Date(format(max_d, "%Y-%m-01")),
                      by = "1 month")
  month_labels <- format(month_breaks, "%B") 
  
  seuil_pluie <- 5
  
  # --- Labels de date au-dessus des barres de pluie (> seuil_pluie)
  rain_labels_df <- if (afficher_dates) {
    df |>
      dplyr::filter(!is.na(rain_mm), rain_mm > seuil_pluie) |>
      dplyr::transmute(
        meteo_datetime,
        y = rain_mm / 2 * 1.02,
        label = format(meteo_datetime, "%d/%m")
      )
  } else {
    df[0, c("meteo_datetime")]
  }
  
  # ---- Préparation des dates de traitement (si disponibles)
  treat_df <- NULL
  if (!is.null(self$metadata) && !is.null(self$metadata$ppp)) {
    ppp <- self$metadata$ppp
    nms <- tolower(names(ppp))
    if ("p_app_date" %in% nms) {
      tdt <- to_posix(ppp[[ which(nms == "p_app_date") ]])
      tdt <- as.Date(tdt)
      tdt <- tdt[!is.na(tdt)]
      if (!is.null(date_debut)) tdt <- tdt[tdt >= date_debut]
      if (!is.null(date_fin))   tdt <- tdt[tdt <= date_fin]
      tdt <- unique(tdt)
      if (length(tdt) > 0) {
        treat_df <- data.frame(meteo_datetime = sort(tdt))
      }
    }
  }
  
  # ---- Construction du graphique
  p <- ggplot2::ggplot(df, ggplot2::aes(x = meteo_datetime)) +
    ggplot2::geom_col(
      ggplot2::aes(y = rain_mm / 2),
      fill = "#223b7b", alpha = 0.6, width = 0.9, na.rm = TRUE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = air_tmin_celsius,  color = "Tmin (°C)"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = air_tmean_celsius, color = "Tmoy (°C)"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = air_tmax_celsius,  color = "Tmax (°C)"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::scale_y_continuous(
      name     = "Température (°C)",
      labels   = scales::label_number(accuracy = 1),
      sec.axis = ggplot2::sec_axis(~ . * 2, name = "Pluie (mm)", breaks = breaks_mm_droit),
      expand   = ggplot2::expansion(mult = c(0.05, 0.16))
    ) +
    ggplot2::scale_x_date(breaks = month_breaks, labels = month_labels) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Tmin (°C)" = "#66b2ff", "Tmoy (°C)" = "#ffcc33", "Tmax (°C)" = "#cc3333", "Traitement" = "black", "Observation" = "black"),
      breaks = c("Tmin (°C)", "Tmoy (°C)", "Tmax (°C)", "Traitement", "Observation")
    )+
    ggplot2::labs(title = "Météo (échelle ombrothermique P = 2T)", x = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position    = "top",
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 8)),
      axis.text.x        = ggplot2::element_text(angle = 0, vjust = 1, hjust = 0.5),
      plot.margin        = ggplot2::margin(b = 24, l = 6, r = 6, t = 6)
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  # --- Ajout des labels de date au-dessus des barres de pluie
  if ((afficher_dates) && nrow(rain_labels_df) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = rain_labels_df,
        mapping = ggplot2::aes(x = meteo_datetime, y = y, label = label),
        inherit.aes = FALSE,
        vjust = -0.1, hjust = 0.5, angle = 0, size = 3, alpha = 0.95
      )
  }
  
  # --- Détermine une "ligne du haut" commune pour placer triangles (traitements) et points (observations)
  tmax <- suppressWarnings(max(df$air_tmax_celsius, na.rm = TRUE))
  tmin <- suppressWarnings(min(df$air_tmin_celsius, na.rm = TRUE))
  if (!is.finite(tmax)) tmax <- suppressWarnings(max(df$air_tmean_celsius, na.rm = TRUE))
  if (!is.finite(tmin)) tmin <- suppressWarnings(min(df$air_tmean_celsius, na.rm = TRUE))
  rng  <- if (is.finite(tmax) && is.finite(tmin)) max(1, tmax - tmin) else 10
  y_top <- if (is.finite(tmax)) tmax + 0.08 * rng else 5
  y_obs <- y_top - 0.06 * rng  # juste en-dessous des triangles pour éviter le chevauchement
  
  # ---- Ajout des points de traitement (triangles)
  if (afficher_traitements && !is.null(treat_df)) {
    p <- p +
      ggplot2::geom_point(
        data = transform(treat_df, y = y_top, label = "Traitement"),
        mapping = ggplot2::aes(x = meteo_datetime, y = y, color = label),
        inherit.aes = FALSE,
        shape = 17, size = 3, alpha = 0.95
      )
    attr(p, "treatment_dates_used") <- treat_df$meteo_datetime
  }
  
  # ---- Récupération et ajout des points d'observation (shape 16), depuis tous les df de self$obs_data
  if (afficher_observations && !is.null(self$obs_data) && length(self$obs_data) > 0) {
    get_one <- function(d) {
      if (is.data.frame(d) && "observation_date" %in% names(d)) {
        dd <- as.Date(to_posix(d$observation_date))
        dd <- dd[!is.na(dd)]
        if (!is.null(date_debut)) dd <- dd[dd >= date_debut]
        if (!is.null(date_fin))   dd <- dd[dd <= date_fin]
        return(dd)
      }
      return(NULL)
    }
    all_dates <- unique(as.Date(unlist(lapply(self$obs_data, get_one))))
    all_dates <- all_dates[!is.na(all_dates)]
    if (length(all_dates) > 0) {
      obs_df <- data.frame(meteo_datetime = sort(all_dates), y = y_obs)
      p <- p +
        ggplot2::geom_point(
          data = transform(obs_df, label = "Observation"),
          mapping = ggplot2::aes(x = meteo_datetime, y = y, color = label),
          inherit.aes = FALSE,
          shape = 16, size = 2.8, alpha = 0.95
        )
      attr(p, "observation_dates_used") <- obs_df$meteo_datetime
    }
  }
  
  attr(p, "data_used") <- df
  p
}


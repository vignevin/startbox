#' @title Plot a Heatmap of Experimental Data
#'
#' @description
#' Generates a heatmap based on experimental observation data,
#' using `plot_x` and `plot_y` coordinates and coloring according to a selected disease variable (_PC columns).
#'
#' @param data A dataframe containing at least 'plot_x', 'plot_y', *_PC variable (e.g., PM_LEAF_PC) and a column plot_id
#' @param titre (optional) Title of the plot. If NULL, a default title based on the variable is used.
#' @param echelle (optional) Maximum value for the fill color scale. If NULL, an automatic scale is calculated.
#' @param orientation (optional) rotation of heatmap c(vertical, horizontal) default vertical
#' @param caption A string used as the caption text displayed at the bottom of the plot. Default is "IFV+".
#'
#' @return A ggplot2 heatmap object.
#'
#' @details
#' - Aggregates values by `plot_id`, taking the mean when multiple observations exist.
#' - If no *_PC variable is found in the data, the function will stop with an error.
#' - Useful for visualizing spatial patterns of disease in experimental trials.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot_xpheat <- function(data, variable, titre = NULL, echelle = NULL,
                        orientation = c("vertical", "horizontal"),
                        residus = FALSE,
                        fill = NULL,
                        caption = "IFV+") {

  orientation <- match.arg(orientation)

  # Check columns
  required_cols <- c("plot_id", "plot_x", "plot_y", variable)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(paste("Missing required column(s):", paste(missing, collapse = ", ")))
  }

  # Cleaning contact details
  data <- dplyr::mutate(data,
                        plot_x = as.numeric(plot_x),
                        plot_y = as.numeric(plot_y)) %>%
    dplyr::filter(!is.na(plot_x) & !is.na(plot_y))

  data_agg <- dplyr::group_by(data, plot_id, plot_x, plot_y) %>%
    dplyr::summarise(Valeurs = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")

  if (nrow(data_agg) == 0) {
    stop("No aggregated data found.")
  }

  # Waste management
  if (residus) {
    moyenne_globale <- mean(data_agg$Valeurs, na.rm = TRUE)
    data_agg <- dplyr::mutate(data_agg, Valeurs = Valeurs - moyenne_globale)
    if (is.null(titre)) titre <- paste("Heatmap - Residuals of", variable)
  } else {
    if (is.null(titre)) titre <- paste("Heatmap -", variable)
  }

  # Automatic scale
  if (is.null(echelle)) {
    max_abs <- max(abs(data_agg$Valeurs), na.rm = TRUE)
    echelle <- ceiling(max_abs * 1.1)
  }

  # Axes
  if (orientation == "vertical") {
    aes_x <- rlang::sym("plot_x")
    aes_y <- rlang::sym("plot_y")
    axis_breaks <- list(
      ggplot2::scale_x_continuous(breaks = seq(min(data_agg$plot_x, na.rm = TRUE), max(data_agg$plot_x, na.rm = TRUE), by = 1)),
      ggplot2::scale_y_continuous(breaks = seq(min(data_agg$plot_y, na.rm = TRUE), max(data_agg$plot_y, na.rm = TRUE), by = 1))
    )
  } else {
    aes_x <- rlang::sym("plot_y")
    aes_y <- rlang::sym("plot_x")
    axis_breaks <- list(
      ggplot2::scale_x_continuous(breaks = seq(min(data_agg$plot_y, na.rm = TRUE), max(data_agg$plot_y, na.rm = TRUE), by = 1)),
      ggplot2::scale_y_continuous(breaks = seq(min(data_agg$plot_x, na.rm = TRUE), max(data_agg$plot_x, na.rm = TRUE), by = 1))
    )
  }

  # Heatmap
  p <- ggplot2::ggplot(data_agg, ggplot2::aes(x = !!aes_x, y = !!aes_y, fill = Valeurs)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = plot_id), color = "white", size = 4) +
    ggplot2::labs(
      title = titre,
      fill = if (residus) "Residues" else "Value (%)",
      caption = caption
    ) +
    {
      if (!is.null(fill) && length(fill) == 2 && !residus) {
        ggplot2::scale_fill_gradient(low = fill[1], high = fill[2],
                                     limits = c(0, echelle),
                                     name = "Average Intensity Value (%)")
      } else if (residus) {
        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                      midpoint = 0, limits = c(-echelle, echelle),
                                      name = "Residues")
      } else {
        ggplot2::scale_fill_gradient(low = "yellow", high = "red",
                                     limits = c(0, echelle),
                                     name = "Average Intensity Value (%)")
      }
    } +
    axis_breaks +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right",
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  return(p)
}



#' Barplot summary for an experiment
#'
#' @description
#' This function automatically generates a bar graph from observation or experimental data.
#'
#' @param data2plot a dataframe, the first col is used for x axis, col value for y axis and col type for fill.
#' @param xcol col to be used as factor for x avis
#' @param ycol col to be used for y axis
#' @param fillcol col to be used for fill
#' @param scale Upper limit of the y axis. If NULL, automatic adjustment.
#' @param bar_color Bar fill colour.
#' @param bar_width bar width
#' @param border_tnt if true, the border color is set as red for TNT
#' @param ... other parameters for labs (title, x, y,fill)
#'
#' @return a barplot
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export

plot_xpbar2 <- function(data2plot,
                        xcol = "xp_trt_code",
                        ycol = "value",
                        fillcol = NULL,
                        scale = NULL,
                        bar_color = NULL,
                        bar_width = 0.7,
                        border_tnt = TRUE,
                        show_errorbar = TRUE,
                        ...) {

# Check that the xcol column exists
  if (!xcol %in% names(data2plot)) {
    stop(paste("Column", xcol, "not found in data"))
  }

  if (!ycol %in% names(data2plot)) {
    stop(paste("Column", ycol, "not found in data"))
  }

  xcol_sym <- rlang::sym(xcol)
  ycol_sym <- rlang::sym(ycol)

  if (!is.null(fillcol))
  {
    fillcol_sym <- rlang::sym(fillcol)
  } else {fillcol_sym <- NULL}

  # Choix intelligent des couleurs si non fourni
  if (is.null(bar_color)) {
    if ("variable" %in% names(data2plot) &&
        any(grepl("LEAF", data2plot$calculation, ignore.case = TRUE))) {
      bar_color <- c("#00AB50", "#66CDAA")  # vert
    } else {
      bar_color <- c("#9966CC", "#CC99FF")  # violet
    }
  }

  # Calculation of scale y
  if (is.null(scale)) {
    max_val <- max(data2plot[,ycol], na.rm = TRUE)
    scale <- ceiling(max_val * 1.1)
  }

  # Create the graph
  p <- ggplot2::ggplot(data2plot, ggplot2::aes(x = !!xcol_sym, y = !!ycol_sym, fill = !!fillcol_sym)) +
    ggplot2::geom_bar(
      ggplot2::aes(color = grepl("TNT", !!xcol_sym)), # logical test in aes
      stat = "summary", fun = "mean",
   position = ggplot2::position_dodge(width = bar_width),
   width = bar_width,
   linewidth = 0.5
   )

  if (border_tnt) {
    p <- p + ggplot2::scale_color_manual(values = c("FALSE" = "grey30", "TRUE" = "red"))
  } else {
    p <- p + ggplot2::scale_color_manual(values = c("FALSE" = "grey30", "TRUE" = "grey30"))
  }

    # Conditional addition of error bars
   if (show_errorbar) {
   p <- p + ggplot2::stat_summary(fun.data = ggplot2::mean_se, geom = "errorbar", width = 0.2)
   }

    # Add text values
   p <- p +
     ggplot2::stat_summary(fun = mean, geom = "text",
                           ggplot2::aes(label = round(..y.., 1)),
                  vjust = -0.5, size = 5)

    # finalization
   p <- p +
     ggplot2::labs(...) +
     ggplot2::ylim(0, scale) +
     #ggplot2::scale_fill_manual(values = bar_color) + ## TO CHECK ... OVERWRITE FILL COLOR IF fillcol provided
     #ggplot2::scale_color_identity() + ## TO CHECK UTILITY... OVERWRITE COLOR BAR RED TNT
     ggplot2::theme_minimal() +
     ggplot2::theme(
   plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
   legend.position = if (length(unique(data2plot$calculation)) > 1) "right" else "none"
   )

  return(p)
}


#' Boxplot summary for an experiment
#'
#' @description
#' This function automatically generates a box plot

#(boxplot) from raw experimental data (plot, observation, disease).
#'
#' @param data Raw data containing at least plot_id, xp_trt_code and a "_PC" variable.
#' @param echelle Upper limit of the y-axis (optional, otherwise automatic).
#' @param ... other parameters for labs (title, x, y,fill)
#'
#' @return A `ggplot2` object.
#'
#' @export

plot_xpbox <- function(data, echelle = NULL, show_dots = FALSE, ...) {

    if (!"plot_id" %in% names(data)) {
    stop("The column 'plot_id' is missing from your data.")
    }

  if (!"xp_trt_code" %in% names(data)) {
    stop("The column 'xp_trt_code' is missing from your data.")
  }

  data$plot_id <- trimws(data$plot_id)

  known_vars <- c("PM_LEAF_PC", "PM_BER_PC", "UN_LEAF_PC", "UN_BER_PC")
  candidate_vars <- intersect(known_vars, names(data))

  if (length(candidate_vars) == 0) {
    stop("No valid _PC variable found in the data.")
  }
  if (length(candidate_vars) > 1) {
    message("Several _PC variables found: ", paste(candidate_vars, collapse = ", "))
    message("The first variable detected is used: ", candidate_vars[1])
  }

  variable <- candidate_vars[1]
  data$Valeurs <- data[[variable]]

  if (is.null(echelle)) {
    max_val <- max(data$Valeurs, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = xp_trt_code, y = Valeurs)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = xp_trt_code),
      alpha = 0.6, outlier.color = "red", outlier.shape = NA
    )

  # ✅ Ajout des points individuels si demandé
  if (show_dots) {
    p <- p + ggplot2::geom_jitter(
      color = "#FF6600",
      width = 0.2, size = 2, alpha = 0.7, show.legend = FALSE
    )
  }

  # Moyenne
  p <- p + ggplot2::stat_summary(
    fun = mean, geom = "point", shape = 4, size = 3, color = "black"
  ) +
    ggplot2::labs(...) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, echelle)) +
    ggplot2::scale_fill_manual(values = rep("#00AB50", length(unique(data$xp_trt_code)))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "none")

  return(p)
}


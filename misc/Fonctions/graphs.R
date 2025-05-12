#' @title Plot a Heatmap of Experimental Data
#'
#' @description
#' Generates a heatmap based on experimental observation data,
#' using `plot_x` and `plot_y` coordinates and coloring according to a selected disease variable (_PC columns).
#'
#' @param data A dataframe containing at least 'plot_x', 'plot_y', *_PC variable (e.g., PM_LEAF_PC) and a column plot_id
#' @param titre (optional) Title of the plot. If NULL, a default title based on the variable is used.
#' @param echelle (optional) Maximum value for the fill color scale. If NULL, an automatic scale is calculated.
#' @param orientation (optional) rotation of heatmap c(“vertical”, “horizontal”) default vertical
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
#' @examples
#' df <- mydata$prepare_final_data()
#' plot_xpheat(df)
#'
#'
# METTRE VARIABLE EN PARAMETRE ENTREE
# AJOUTER PLOT_ID DANS LE CHECK

plot_xpheat <- function(data, variable, titre = NULL, echelle = NULL,
                        orientation = c("vertical", "horizontal"),
                        residus = FALSE,
                        fill = NULL,
                        caption = "IFV+") {
  
  orientation <- match.arg(orientation)
  
  # Vérification colonnes
  required_cols <- c("plot_id", "plot_x", "plot_y", variable)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(paste("❌ Missing required column(s):", paste(missing, collapse = ", ")))
  }
  
  # Nettoyage des coordonnées
  data <- dplyr::mutate(data,
                        plot_x = as.numeric(plot_x),
                        plot_y = as.numeric(plot_y)) %>%
    dplyr::filter(!is.na(plot_x) & !is.na(plot_y))
  
  # Agrégation
  data_agg <- dplyr::group_by(data, plot_id, plot_x, plot_y) %>%
    dplyr::summarise(Valeurs = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")
  
  if (nrow(data_agg) == 0) {
    stop("❌ No aggregated data found.")
  }
  
  # Gestion des résidus
  if (residus) {
    moyenne_globale <- mean(data_agg$Valeurs, na.rm = TRUE)
    data_agg <- dplyr::mutate(data_agg, Valeurs = Valeurs - moyenne_globale)
    if (is.null(titre)) titre <- paste("Heatmap - Residuals of", variable)
  } else {
    if (is.null(titre)) titre <- paste("Heatmap -", variable)
  }
  
  # Échelle automatique
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
      fill = if (residus) "Résidus" else "Valeur (%)",
      caption = caption
    ) +
    {
      if (!is.null(fill) && length(fill) == 2 && !residus) {
        ggplot2::scale_fill_gradient(low = fill[1], high = fill[2],
                                     limits = c(0, echelle),
                                     name = "Valeur Moyenne Intensité (%)")
      } else if (residus) {
        ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                      midpoint = 0, limits = c(-echelle, echelle),
                                      name = "Résidu")
      } else {
        ggplot2::scale_fill_gradient(low = "yellow", high = "red",
                                     limits = c(0, echelle),
                                     name = "Valeur Moyenne Intensité (%)")
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



#' Barplot résumé pour une expérimentation
#'
#' @description
#' Cette fonction génère automatiquement un graphique en barres à partir de données d'observations ou expérimentales.
#'
#' @param data2plot a dataframe, the first col is used for x axis, col "value" for y axis and col "type" for fill.
#' @param echelle Limite supérieure de l'axe y. Si NULL, ajustement automatique.
#' @param couleur_bars Couleur de remplissage des barres.
#' @param bar_width bar width
#' @param border_tnt if true, the border color is set as red for TNT
#' @param ... other parameters for labs (title, x, y,fill)
#'
#' @return a barplot
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' # Exemple d'utilisation
#' plot_xpbar(my_data)
#

library(ggplot2)
library(dplyr)
plot_xpbar2 <- function(data2plot,
                        xcol = "trt_code",
                        echelle = NULL,
                        couleur_bars = NULL,
                        bar_width = 0.7,
                        border_tnt = TRUE,
                        show_errorbar = TRUE,
                        ...) {
  
  # Vérification que la colonne xcol existe
  if (!xcol %in% names(data2plot)) {
    stop(paste("Column", xcol, "not found in data"))
  }
  
  xcol_sym <- rlang::sym(xcol)
  
  # Choix intelligent des couleurs si non fourni
  if (is.null(couleur_bars)) {
    if ("variable" %in% names(data2plot) &&
        any(grepl("LEAF", data2plot$variable, ignore.case = TRUE))) {
      couleur_bars <- c("#00AB50", "#66CDAA")  # vert
    } else {
      couleur_bars <- c("#9966CC", "#CC99FF")  # violet
    }
  }
  
  # Couleur des bordures : rouge si TNT, sinon gris
  data2plot <- data2plot %>%
    dplyr::mutate(
      border_color = if (border_tnt) {
        ifelse(!!xcol_sym == "TNT", "red", "grey30")
      } else {
        "grey30"
      }
    )
  
  # Calcul de l’échelle y
  if (is.null(echelle)) {
    max_val <- max(data2plot$value, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)
  }
  
  # Création du graphique
  p <- ggplot(data2plot, aes(x = !!xcol_sym, y = value, fill = type)) +
    geom_bar(
      aes(color = border_color),
      stat = "identity",
      position = position_dodge(width = bar_width),
      width = bar_width,
      linewidth = 0.5
    )
  
  # Ajout conditionnel des barres d'erreur
  if (show_errorbar) {
    p <- p + geom_errorbar(
      data = filter(data2plot, !is.na(lower.CL) & !is.na(upper.CL)),
      aes(ymin = lower.CL, ymax = upper.CL),
      position = position_dodge(width = bar_width),
      width = 0.2
    )
  }
  
  # Ajout des valeurs texte
  p <- p +
    geom_text(
      aes(label = round(value, 1)),
      vjust = -0.5, size = 4, color = "black",
      position = position_dodge(width = bar_width)
    ) +
    labs(...) +
    ylim(0, echelle) +
    scale_fill_manual(values = couleur_bars) +
    scale_color_identity() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = if (length(unique(data2plot$type)) > 1) "right" else "none"
    )
  
  return(p)
}



#' Boxplot résumé pour une expérimentation
#'
#' @description
#' Cette fonction génère automatiquement un graphique en boîte à moustaches
#' (boxplot) à partir de données expérimentales brutes (parcelle, observation, maladie).
#'
#' @param data Données brutes contenant au moins plot_id et une variable "_PC".
#' @param titre Titre principal du graphique (optionnel).
#' @param echelle Limite supérieure de l'axe y (optionnel, sinon automatique).
#' @param caption A string used as the caption text displayed at the bottom of the plot. Default is "IFV+".
#'
#' @return Un objet `ggplot2`.
#' @export
#'
#' @examples
#' plot_xpbox(my_data)


plot_xpbox <- function(data, titre = NULL, echelle = NULL, caption = "IFV+") {
  library(ggplot2)
  library(dplyr)
  
  if (!"plot_id" %in% names(data)) {
    stop("La colonne 'plot_id' est manquante dans vos données.")
  }
  
  data$plot_id <- trimws(data$plot_id)
  
  known_vars <- c("PM_LEAF_PC", "PM_BER_PC", "UN_LEAF_PC", "UN_BER_PC")
  candidate_vars <- intersect(known_vars, names(data))
  
  if (length(candidate_vars) == 0) {
    stop("Aucune variable _PC valide trouvée dans les données.")
  }
  if (length(candidate_vars) > 1) {
    message("Plusieurs variables _PC trouvées : ", paste(candidate_vars, collapse = ", "))
    message("La première variable détectée est utilisée : ", candidate_vars[1])
  }
  
  variable <- candidate_vars[1]
  
  # Utiliser remove_block_code() à la place du split fait main
  data$plot_id <- toupper(trimws(data$plot_id))
  blocks <- c("A", "B", "C", "D")
  data$xp_trt_code <- remove_block_code(data$plot_id, blocks = blocks)
  # Créer la colonne Valeurs
  data <- data %>%
    mutate(
      Valeurs = as.numeric(.data[[variable]]),
      Type = "Intensite"
    )
  
  # Échelle automatique si non précisée
  if (is.null(echelle)) {
    max_val <- max(data$Valeurs, na.rm = TRUE)
    echelle <- ceiling(max_val * 1.1)
  }
  
  if (is.null(titre)) {
    titre <- paste("Boxplot -", variable)
  }
  
  # Graphique
  p <- ggplot(data, aes(x = xp_trt_code, y = Valeurs)) +
    geom_boxplot(
      aes(fill = xp_trt_code),
      alpha = 0.6, outlier.color = "red", outlier.shape = NA
    ) +
    stat_summary(
      fun = mean, geom = "point", shape = 4, size = 3, color = "black"
    ) +
    labs(
      title = titre,
      subtitle = paste("Date :", paste(unique(data$observation_date), collapse = ", ")),
      x = "Traitement",
      y = "Intensité (%)",
      caption = caption
    ) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, echelle)) +
    scale_fill_manual(
      values = rep("#00AB50", length(unique(data$xp_trt_code)))
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  return(p)
}


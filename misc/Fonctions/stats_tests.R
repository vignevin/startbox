#' Perform ANOVA or Kruskal-Wallis test and group comparison
#'
#' This function performs an ANOVA or Kruskal-Wallis test depending on the assumptions of normality and homoscedasticity.
#' It then returns a summary of statistical groups using a post-hoc SNK test (for ANOVA) or Kruskal-Conover test.
#'
#' @param data A data.frame containing the experimental data. Must contain either a `xp_trt_code` column or a `plot_id` column to infer it.
#' @param value_col A string specifying the name of the column containing the numeric values to be tested (e.g., PM_LEAF_PC,PM_BER_PC,UN_LEAF_PC,UN_BER_PC).
#'
#' @return A list with the following elements:
#'  {test} A string indicating the test used: "Anova", "Kruskal", or "Non applicable".
#'  {p_value} The p-value of the global test.
#'  {groupes} A data.frame with three columns: `modality`, `mean`, and `groups`. The `groups` column contains statistical group letters.
#' }
#'
#' @details
#' If `xp _trt_code` is not provided, it is inferred from the `plot_id` column:
#' numeric plot codes are extracted from the beginning of the string (e.g. "10A" → 10),
#' and "TNT" is used directly for untreated control plots.
#' 
#' Group comparison is only meaningful if the global test (ANOVA or Kruskal-Wallis) is statistically significant.
#'
#' @importFrom stats aov kruskal.test aggregate as.formula residuals shapiro.test bartlett.test
#' @importFrom agricolae SNK.test kruskal
#'
#' @param data A data.frame with at least a column of values and a grouping factor.
#' @param value_col Column name (string) with numeric values.
#' @param group_col Column name (string) with grouping (default: "xp_trt_code").
#' @param alpha Significance threshold (default = 0.05).
#' @param group_method "SNK" or "LSD" for ANOVA; always "kruskal" for Kruskal-Wallis.
#' @param force_test Force "anova" or "kruskal" (bypass assumptions).
#' @param verbose Logical. If TRUE, prints diagnostic messages.
#'
#' @return A list with test name, p-value and group letters.
#' @export
test_stats <- function(data, value_col = "PM_LEAF_PC", group_col = "xp_trt_code",
                       alpha = 0.05,
                       group_method = "SNK",
                       force_test = NULL,
                       verbose = TRUE) {
  
  # Vérification
  if (!(value_col %in% names(data)) || !(group_col %in% names(data))) {
    stop("❌ Required columns are missing.")
  }
  
  df <- data[!is.na(data[[value_col]]) & !is.na(data[[group_col]]), ]
  if (nrow(df) == 0 || length(unique(df[[group_col]])) <= 1) {
    if (verbose) message("⚠️ Not enough data or only one group.")
    return(list(test = "Non applicable", p_value = NA, groupes = data.frame(), message = "Not enough data"))
  }
  
  df[[group_col]] <- as.factor(df[[group_col]])
  formula <- stats::as.formula(paste(value_col, "~", group_col))
  
  # Préparation des valeurs par défaut
  test_type <- NA
  pval <- NA
  groupes <- data.frame()
  justification <- ""
  
  if (is.null(force_test)) {
    model <- stats::aov(formula, data = df)
    p_norm <- as.character(check_normality(model))
    p_var <- as.character(check_heteroscedasticity(model))
    
    if (verbose) message("🔍 Shapiro p =", round(p_norm, 3), "| Bartlett p =", round(p_var, 3))
    
    if (p_norm > alpha && p_var > alpha) {
      test_type <- "Anova"
      justification <- paste("✅ ANOVA used (Shapiro p =", round(p_norm, 3),
                             ", Bartlett p =", round(p_var, 3), ") - assumptions respected.")
    } else {
      test_type <- "Kruskal"
      justification <- paste("🔄 Kruskal-Wallis used (Shapiro p =", round(p_norm, 3),
                             ", Bartlett p =", round(p_var, 3), ") - assumptions not met.")
    }
    
  } else if (tolower(force_test) == "anova") {
    test_type <- "Anova"
    model <- stats::aov(formula, data = df)
    justification <- "⚠️ ANOVA forced by user."
  } else if (tolower(force_test) == "kruskal") {
    test_type <- "Kruskal"
    justification <- "⚠️ Kruskal-Wallis forced by user."
  } else {
    stop("❌ Invalid value for force_test. Use 'anova' or 'kruskal'.")
  }
  
  # Calcul du test global et des groupes
  if (test_type == "Anova") {
    model <- if (exists("model")) model else stats::aov(formula, data = df)
    pval <- summary(model)[[1]][["Pr(>F)"]][1]
    
    groupes <- switch(
      group_method,
      SNK = agricolae::SNK.test(model, trt = group_col, group = TRUE)$groups,
      LSD = agricolae::LSD.test(model, trt = group_col, group = TRUE)$groups,
      stop("❌ Invalid group_method for ANOVA.")
    )
    
  } else if (test_type == "Kruskal") {
    pval <- stats::kruskal.test(formula, data = df)$p.value
    groupes <- agricolae::kruskal(df[[value_col]], df[[group_col]], group = TRUE)$groups
  }
  
  # Construction des résultats
  groupes$modality <- rownames(groupes)
  moyennes <- stats::aggregate(df[[value_col]], by = list(df[[group_col]]), FUN = mean)
  names(moyennes) <- c("modality", "mean")
  
  groupes <- merge(groupes, moyennes, by = "modality", all.x = TRUE)
  groupes <- groupes[order(groupes$groups, groupes$mean), ]
  rownames(groupes) <- groupes$modality
  groupes <- groupes[, c("modality", "mean", "groups")]
  
  return(list(
    test = test_type,
    p_value = pval,
    groupes = groupes,
    message = justification
  ))
}

# Test de normalité des résidus (Shapiro-Wilk)
check_normality <- function(model) {
  stats::shapiro.test(residuals(model))$p.value
}

# Test d’homogénéité des variances (Bartlett sur les résidus)
check_heteroscedasticity <- function(model) {
  data <- model$model
  stats::bartlett.test(data[[1]] ~ data[[2]])$p.value
}


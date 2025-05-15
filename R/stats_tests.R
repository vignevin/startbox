#' Perform ANOVA or Kruskal-Wallis test and group comparison
#'
#' This function performs an ANOVA or Kruskal-Wallis test depending on the assumptions of normality and homoscedasticity.
#' It then returns a summary of statistical groups using a post-hoc SNK test (for ANOVA) or Kruskal-Conover test.
#'
#' @param data A data.frame containing the experimental data. Must contain either a `xp_trt_code` column or a `plot_id` column to infer it.
#' @param value_col A string specifying the name of the column containing the numeric values to be tested (e.g., PM_LEAF_PC,PM_BER_PC,UN_LEAF_PC,UN_BER_PC).
#'
#' @return A list with the following elements:
#'  - test A string indicating the test used: "Anova", "Kruskal", or "Non applicable".
#'  - p_value The p-value of the global test.
#'  - groupes A data.frame with three columns: `modality`, `mean`, and `groups`. The `groups` column contains statistical group letters.
#'
#'
#' @details
#' If `xp _trt_code` is not provided, it is inferred from the `plot_id` column:
#' numeric plot codes are extracted from the beginning of the string (e.g. "10A" to 10),
#' and "TNT" is used directly for untreated control plots.
#'
#' Group comparison is only meaningful if the global test (ANOVA or Kruskal-Wallis) is statistically significant.
#'
#' @importFrom stats aov kruskal.test aggregate as.formula residuals shapiro.test bartlett.test
#' @importFrom agricolae SNK.test kruskal
#'
#' @param data A data.frame with at least a column of values and a grouping factor.
#' @param value_col Column name (string) with numeric values.
#' @param trt_col Column name (string) with grouping (default: "xp_trt_code").
#' @param alpha Significance threshold (default = 0.05).
#' @param group_method "SNK" or "LSD" for ANOVA; always "kruskal" for Kruskal-Wallis.
#' @param force_test Force "anova" or "kruskal" (bypass assumptions).
#' @param verbose Logical. If TRUE, prints diagnostic messages.
#'
#' @return A list with test name, p-value and group letters.
#' @export
test_stats <- function(data, value_col = "PM_LEAF_PC", trt_col = "xp_trt_code",
                       alpha = 0.05,
                       group_method = "SNK",
                       force_test = NULL,
                       verbose = TRUE) {

  if (!(value_col %in% names(data)) || !(trt_col %in% names(data))) {
    stop("Required columns are missing.")
  }

  df <- data[!is.na(data[[value_col]]) & !is.na(data[[trt_col]]), ]

  if (nrow(df) == 0 || length(unique(df[[trt_col]])) <= 1) {
    if (verbose) message("Not enough data or only one group.")
    return(list(test = "Non applicable", p_value = NA, groupes = data.frame(groups = character(0))))
  }

  df[[trt_col]] <- as.factor(df[[trt_col]])
  formula <- as.formula(paste(value_col, "~", trt_col))

  if (is.null(force_test)) {
    model <- stats::aov(formula, data = df)
    p_norm <- check_normality(model)
    p_var <- check_heteroscedasticity(model)
    if (verbose) message("Shapiro p =", round(p_norm, 3), " | Bartlett p =", round(p_var, 3))
  }

  if (!is.null(force_test) && force_test == "anova" ||
      is.null(force_test) && p_norm > alpha && p_var > alpha) {
    test_type <- "Anova"
    model <- stats::aov(formula, data = df)
    pval <- summary(model)[[1]][["Pr(>F)"]][1]

    groupes <- switch(
      group_method,
      SNK = agricolae::SNK.test(model, trt = trt_col, group = TRUE)$groups,
      LSD = agricolae::LSD.test(model, trt = trt_col, group = TRUE)$groups,
      stop("Invalid group_method for ANOVA.")
    )

  } else {
    test_type <- "Kruskal"
    pval <- stats::kruskal.test(formula, data = df)$p.value
    groupes <- agricolae::kruskal(df[[value_col]], df[[trt_col]], group = TRUE)$groups
  }

  groupes$modality <- rownames(groupes)
  moyennes <- stats::aggregate(df[[value_col]], by = list(df[[trt_col]]), FUN = mean)
  names(moyennes) <- c("modality", "mean")

  groupes <- merge(groupes, moyennes, by = "modality")
  groupes <- groupes[order(groupes$groups, groupes$mean), ]
  rownames(groupes) <- groupes$modality
  groupes <- groupes[, c("modality", "mean", "groups")]

  return(list(test = test_type, p_value = pval, groupes = groupes))
}

# Residual normality test (Shapiro-Wilk)
check_normality <- function(model) {
  stats::shapiro.test(residuals(model))$p.value
}

# Homogeneity of variances test (Bartlett on residuals)
check_heteroscedasticity <- function(model) {
  data <- model$model
  stats::bartlett.test(data[[1]] ~ data[[2]])$p.value
}


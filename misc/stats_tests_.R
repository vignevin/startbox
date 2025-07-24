# this will be ok for one variable at a time/ need to have a ready_data with calculation for several variable...to think about it also...

# test test_stats_  (add checks)
# improve prepare_data when severals variables are in raw data

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
#' @param self An instance of the `UserData` R6 class containing metadata$moda_desc
#' @param prep_data character, the name of a dataframe inside self$prepared_data.
#' @param fcol col name as factor. NULL by default, if not provided xp_trt_name or xp_trt_code is used
#' @param block colname to use as block. if FALSE : no block factor tested. if TRUE : "plot_block" is used. if value : value is used
#' @param alpha Significance threshold (default = 0.05).
#' @param p.adj Method for adjusting p values. see agricolae::LSD.test(). “none”, “holm”, “hommel”, “hochberg”, “bonferroni”, “BH”, “BY”, “fdr”
#' @param force_test Force "anova" or "kruskal" (bypass assumptions).
#'
#' @return A list with test name, p-value and group letters.
#' @export
test_stats_ <- function(
  self,
  prep_data = NULL,
  fcol = NULL,
  block = FALSE,
  alpha = 0.05,
  p.adj = "bonferroni",
  force_test = NULL
) {
  # check if self is UserData
  if (!inherits(self, "UserData")) {
    message(
      "Argument self is not an UserData object as required by the function"
    )
    return(NULL)
  }

  # check if prep_data exists
  if (is.null(self$prepared_data[[prep_data]])) {
    message(paste("no", prep_data, "found in prepared_data. Function aborted"))
    return(NULL)
  }

  data <- self$prepared_data[[prep_data]]

  # a ready data MUST have cols value and calculation
  flag_ready <- all(c("value", "calculation") %in% colnames(data))

  if (!flag_ready) {
    message(paste(
      prep_data,
      "is not a valid prepared_data dataframe (missing value or/and calculation col). Function aborted"
    ))
    return(NULL)
  }

  ## factor trt
  if (is.null(fcol)) {
    fcol <- intersect(c("xp_trt_name", "xp_trt_code"), names(data))[1]
  }
  if (length(fcol) != 1) {
    stop(paste("No - or more than one - factor col found in data"))
  }

  ## bloc
  if (block != FALSE) {
    if (block == TRUE) {
      block <- "plot_block"
    }
  }

  unique_calculation <- unique(data$calculation)

  ## stat for each calculation
  stat_list <- list()
  for (calc in unique_calculation) {
    # write formula
    formula_str <- if (!isFALSE(block)) {
      paste(paste0("`", calc, "`"), "~", fcol, "+", block)
    } else {
      paste(paste0("`", calc, "`"), "~", fcol)
    }
    temp_data <- data[data$calculation == calc, ]
    temp_data <- resume_pivot_wider(temp_data)
    temp_stat <- stat_on_data(
      data = temp_data,
      formula_str = formula_str,
      alpha = alpha,
      p.adj = p.adj,
      force_test = NULL
    )
    stat_list[[calc]] <- temp_stat
  }

  ## combine all grp_means in one dataframe
  # Extract all 'grp_means' elements if they exist
  grp_means_list <- lapply(stat_list, function(x) x[["grp_means"]])
  # Keep only the non-NULL elements (i.e. where 'grp_means' existed)
  grp_means_list <- Filter(Negate(is.null), grp_means_list)
  # Then bind them together
  df.grp_means <- do.call(dplyr::bind_rows, grp_means_list)

  ## combine all stats df in one dataframe
  # Extract all 'grp_means' elements if they exist
  stats_list <- lapply(stat_list, function(x) x[["stats"]])
  # Keep only the non-NULL elements (i.e. where 'stats' existed)
  stats_list <- Filter(Negate(is.null), stats_list)
  # Then bind them together
  df.stats <- do.call(dplyr::bind_rows, stats_list)

  ## stats_result
  stats_result <- list(
    prep_data = prep_data,
    df.stats = df.stats,
    df.grp_means = df.grp_means
  )


  self$stats[[prep_data]] <- stats_result
  message(paste0("Stats saved in ",self$name," : stats$",prep_data))
}


#' Statistic pipeline for data analysis
#'
#' @description
#' This function applies a linear model to a dataset to test the effect of one or more factors.
#' If the conditions of normality and heterosceadiscity are met, an ANOVA test is performed.
#' Otherwise, a non-parametric Kruskal-Walis test is performed.
#'
#'
#' @param data a data.frame with data
#' @param formula_str a string with model formula, as "`FA UN_BER_PC` ~ xp_trt_code"
#' @param alpha alpha value for statistical tests
#' @param force_test NULL, "anova" or "kruskal". By default set to NULL, and if normality or heteroscedasticity are correct, an Anova test is done, otherwise a Kruskal test is done.
#' @param p.adj Method for adjusting p values. see agricolae::LSD.test()
#'
#' @returns a list
#' @export
#'
#' @examples
stat_on_data <- function(
  data,
  formula_str,
  alpha = 0.05,
  force_test = NULL,
  p.adj = "bonferroni"
) {
  formula <- as.formula(formula_str)
  # Extract variable names from the formula
  formula_vars <- all.vars(formula)

  # Get column names from the data frame
  data_vars <- names(data)

  # Check if all variables in the formula are present in the data frame
  missing_vars <- setdiff(formula_vars, data_vars)

  if (length(missing_vars) > 0) {
    stop(paste(
      "The following variables from the formula are not found in the data frame:",
      paste(missing_vars, collapse = ", ")
    ))
  }

  ## to be sure that model is only with factors
  data %>%
    dplyr::mutate_at(formula_vars[-1], as.factor) -> data

  ## check
  predictors <- formula_vars[-1]

  # Vérifier chaque prédicteur pour voir s'il est un facteur et compte le nombre de niveaux
  for (var in predictors) {
    var_name <- as.character(var)
        if (length(levels(data[[var_name]])) < 2) {
          stop("contrasts can be applied only to factors with 2 or more levels. Function aborted")
        }
      }


  # Fit the linear model if all variables are present
  model <- lm(formula, data = data)

  ## diagnostics
  p_norm <- check_normality(model)
  p_var <- check_heteroscedasticity(model)
  plot_qq <- qq_residual_plot(model)

  ## test interaction
  if (length(formula_vars) > 2) {
    formula_str_with_interaction <- gsub("\\+", "*", formula_str)
    model_with_interaction <- lm(
      as.formula(formula_str_with_interaction),
      data = data
    )
    # coefficients
    coeff_table <- summary(model_with_interaction)$coefficients
    # rows interactions (content ":")
    interaction_rows <- grep(":", rownames(coeff_table))
    # Si aucune interaction dans le modèle
    if (length(interaction_rows) == 0) {
      message("No interaction in the model")
    } else {
      message(
        "Warning : interactions detected. Function outputs may be incorrects"
      )
    }
  }

  if (is.null(force_test) || force_test == "anova") {
    force_test <- "anova"
    anova_table <- stats::anova(model)
    ptest <- anova_table[1, 'Pr(>F)']
    ## means for each factor
    grp_means <- data.frame()
    for (f in 2:length(formula_vars)) {
      mf <- agricolae::LSD.test(
        model,
        trt = formula_vars[f],
        alpha = alpha,
        p.adj = p.adj,
        group = TRUE
      )
      mns <- mf$means
      grp <- mf$groups
      mns$factor_level <- rownames(mns)
      mns$factor <- formula_vars[f]
      mns$calculation <- formula_vars[1]
      grp$factor_level <- rownames(grp)
      grp_means <- dplyr::bind_rows(grp_means, merge(mns, grp))
    }
    if (p_norm < alpha | p_var < alpha) {
      force_test <- "kruskal"
    }
  }

  if (!is.null(force_test) && force_test == "kruskal") {
    ## only one factor is allowed
    if (length(formula_vars) > 2) {
      formula_str_short <- sub("\\s*[+*].*", "", formula_str)
      formula <- as.formula(formula_str_short)
    }
    formula_vars <- all.vars(formula)
    kruskal <- stats::kruskal.test(formula, data = data)
    ksk <- agricolae::kruskal(
      data[[formula_vars[1]]],
      data[[formula_vars[2]]],
      p.adj = p.adj,
      group = TRUE
    )
    mns <- ksk$means
    colnames(mns)[grepl("formula_vars", colnames(mns))] <- formula_vars[1]
    grp <- ksk$groups
    colnames(grp)[grepl("formula_vars", colnames(grp))] <- "rank"
    mns$factor_level <- rownames(mns)
    mns$factor <- formula_vars[2]
    mns$calculation <- formula_vars[1]
    grp$factor_level <- rownames(grp)
    grp_means <- merge(mns, grp)
    ptest <- kruskal$p.value
  }

  ## change name
  colnames(grp_means)[grepl(formula_vars[1], colnames(grp_means))] <- "mean"

  ## data.frame to store main stats
  df.stats <- data.frame(
    formula_used = paste(deparse(formula), collapse = ""),
    calculation = formula_vars[1],
    test = force_test,
    ptest = ptest,
    p_norm = p_norm,
    p_var = p_var
  )

  ## to add significance label
  df.stats %>%
    dplyr::mutate(signif = dplyr::case_when(
      ptest <= 0.001 ~ "***",
      ptest <= 0.01  ~ "**",
      ptest <= 0.05  ~ "*",
      ptest >  0.5   ~ "ns",
      TRUE ~ ""
    )) -> df.stats

  return(list(
    stats = df.stats,
    #   plot_qq = plot_qq,
    grp_means = grp_means
  ))
}


#' Residual normality test
#'
#' @description
#' This function test residual normality test thanks to Shapiro-Wilk's test
#'
#' @param model a linear model (lm) object
#' @param alpha alpha value for test
#'
#' @returns pvalue of Shapiro-Wilk's test
#' @export
#'
#' @examples
check_normality <- function(model, alpha = 0.05) {
  pval <- stats::shapiro.test(residuals(model))$p.value
  if (pval < alpha) {
    message(paste(
      "Shapiro test p-value is less than",
      alpha,
      ": the model residuals do not appear to follow a normal distribution"
    ))
  } else {
    message(paste(
      "Shapiro test p-value is greater than",
      alpha,
      ": the model residuals appear to follow a normal distribution"
    ))
  }
  return(pval)
}


#' Bartlett on residuals
#'
#' @description
#' This function test homogeneity of variances test thanks to Bartlett's test on residuals
#'
#' @param model a linear model (lm) object
#' @param alpha alpha value for test
#'
#' @returns pvalue of Bartlett's test
#' @export
#'
#' @examples
check_heteroscedasticity <- function(model, alpha = 0.05) {
  if (!inherits(model, "lm")) {
    stop("Input must be a linear model (lm) object.")
  }

  data <- model$model
  pval <- stats::bartlett.test(data[[1]] ~ data[[2]])$p.value
  if (pval < alpha) {
    message(paste(
      "Bartlett test p-value is less than",
      alpha,
      ": significant differences in variance between groups are detected"
    ))
  } else {
    message(paste(
      "Bartlett test p-value is greater than",
      alpha,
      ": no significant differences in variance between groups detected"
    ))
  }
  return(pval)
}


#' Q-Q plot of residuals
#'
#' @description
#' Function to return a Q-Q plot of residuals for a linear model
#'
#' @param model a linear model (lm) object
#'
#' @returns qq plot of residuals
#' @export
#'
#' @examples
#' # Example usage:
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' qq_residual_plot(fit)
qq_residual_plot <- function(model) {
  if (!inherits(model, "lm")) {
    stop("Input must be a linear model (lm) object.")
  }

  # Extract residuals
  residuals <- resid(model)

  # Create a data frame for plotting
  df <- data.frame(
    sample = residuals
  )

  # Generate Q-Q plot using ggplot2
  p <- ggplot2::ggplot(df, ggplot2::aes(sample = sample)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Q-Q Plot of Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )

  return(p)
}




#' calculation of disease incidence
#'
#' @description
#' This function calculates the frequency of the disease in the observed population,
#' i.e. the number of sampled units (leaves, bunches, plants, etc.) infected expressed
#' as a percentage of the total number of units observed.
#'
#' @param vector a numeric vector of disease severity observations in percentage
#' @param threshold threshold to calculate incidence, zero by default
#'
#' @returns a numeric value of incidence, in percent
#'
#' @examples
#' vec <- c(1, 2, 3, 0, 5, 6, 0)
#' incidence(vec)
#' @export
incidence <- function(vector, threshold = 0) {
  # Check if the input is a numeric vector
  if (!is.numeric(vector)) {
    stop("Input must be a numeric vector.")
  }
  #if(any(is.na(vector))) {message("NA values ignored")}
  # Calculate the incidence of values greater than threshold
  freq <- 100 * sum(vector > threshold, na.rm = T) / sum(!is.na(vector))
  return(freq)
}

#' calculation of disease intensity
#'
#' @description
#' This function calculate the disease intensity, as the mean value of severity measurements.
#' The disease intensity is the amount of disease present in the population,
#' expressed in percentage.
#'
#' @param vecteur a numeric vector of disease severity observations in percentage
#'
#' @returns a numeric value of intensity
#'
#' @examples
#' vec <- c(100, 20, 30, 0, 5, 0, 0)
#' intensity(vec)
#' @export
intensity <- function(vecteur) {
  # Check if the input is a numeric vector
  if (!is.numeric(vecteur)) {
    stop("Input must be a numeric vector.")
  }
  # Calculate intensity
  int <- mean(vecteur, na.rm = T)
  return(int)
}
# TO DO check that values in vecteur are between 0 and 100 ?

#' calculation of disease severity
#'
#' @description
#' This function calculate the disease severity, as the mean value of severity measurements for diseased units ONLY (leaves, bunches, plants...).
#' The disease severity is the area of a sampling unit affected by the disease, expressed as a percentage of the total area.
#'
#' @param vecteur a numeric vector of disease severity observations in percentage
#'
#' @returns a numeric value of intensity
#'
#' @examples
#' vec <- c(100, 20, 30, 0, 5, 0, 0)
#' severity_diseased(vec)
#' @export
severity_diseased <- function(vecteur) {
  # Check if the input is a numeric vector
  if (!is.numeric(vecteur)) {
    stop("Input must be a numeric vector.")
  }
  # Calculate intensity
  if (length(vecteur[vecteur > 0]) > 0) {
    sev <- mean(vecteur[vecteur > 0], na.rm = T)
  } else {
    sev <- 0
  }
  return(sev)
}
# TO DO check that values in vecteur are between 0 and 100 ?

#' Calcul of efficacy according to the Abbott's formula
#' @description
#' This function calculate the treatment efficacy according to the Abbott formula (1925).
#' Eff = ((value_tnt - value)/value_tnt) * 100
#' with value_tnt : percentage of linving in the check (non treated) and value : percentage living in the treatment
#' The Abbott formula is appropriate when dealing with infestation levels or live individuals in uniform pest or disease populations
#' Efficacy is the level of reduction of target harmful organisms or damage caused by them to the plant,
#' after application of a treatment, compared to an untreated control.
#' Efficacy is expressed in percentage.
#' Reference : Abbott, W.S. 1925. A method of computing the effectiveness of an insecticide. J. Econ. Entomol., 18:265-267.
#' See also : Henderson and Tilton (1955) formula for non-uniform populations
#'
#' @param value numeric value
#' @param value_tnt numeric value of TNT
#'
#' @returns a vector of calculated efficacy
#'
#' @export
efficacy <- function(value, value_tnt) {
  eff <- 100 - ((value * 100 / value_tnt))
  ## eff[eff < 0] <- 0 ## negative values seems allowed by Abbott formula
  return(eff)
}

#' Calcul efficacy according to the Henderson and Tilton formula
#' @description
#' This function calculate the treatment efficacy according to the Henderson and Tilton formula (1955).
#' #' For non-uniform populations, the Henderson and Tilton (1955) formula is suited which allows us to judge differences in the
#' population development. In this formula the mortality ratio is corrected on the control mortality
#' Eff = (1 - (T_a * C_b) / (T_b * C_a)) * 100
#' with :
#' Tb number of mites before treatment
#' Ta  number of mites after treatment
#' Cb number of mites before treatment in the check plot (TNT)
#' Ca number of mites after treatment in the check plot (TNT)
#' Efficacy is expressed in percentage.
#'
#' Reference : Henderson, C.F. and E. W. Tilton. 1955. Tests with acaricides against the brow wheat mite, J. Econ. Entomol. 48:157-161.
#' See also : Abbott (1925) formula
#'
#' @param Tb number of mites before treatment (numeric)
#' @param Ta number of mites after treatment (numeric)
#' @param Cb number of mites before treatment in the check plot (TNT) (numeric)
#' @param Ca number of mites after treatment in the check plot (TNT) (numeric)
#'
#' @returns a vector of calculated efficacy
#'
#' @export
#' @examples
#' # ht_efficacy(Ta=5,Tb=15,Ca=20,Cb=15)
#'
ht_efficacy <- function(Ta,Tb,Ca,Cb) {
  eff <- 100 * (1 - (Ta * Cb) / (Tb * Ca))
  return(eff)
}


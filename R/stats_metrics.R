#' calculation of disease incidence
#'
#' @description
#' This function calculates the frequency of the disease in the observed population,
#' i.e. the number of sampled units (leaves, bunches, plants, etc.) infected expressed
#' as a percentage of the total number of units observed.
#'
#' @param vecteur a numeric vector of disease severity observations in percentage
#'
#' @returns a numeric value of incidence, in percent
#'
#' @examples
#' vec <- c(1, 2, 3, 0, 5, 6, 0)
#' incidence(vec)
#' @export
incidence <- function(vecteur) {
  # Check if the input is a numeric vector
  if (!is.numeric(vecteur)) {
    stop("Input must be a numeric vector.")
  }
  # Calculate the incidence of values greater than zero
  freq <- length(vecteur[vecteur > 0]) * 100 / length(vecteur)
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
  int <- mean(vecteur, na.rm=T)
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
  if (length(vecteur[vecteur > 0])>0) {
  sev <- mean(vecteur[vecteur > 0], na.rm=T)
              } else {sev <- 0}
  return(sev)
}
# TO DO check that values in vecteur are between 0 and 100 ?


#' calcul efficacy
#' @description
#' This function calculate the treatment efficacy.
#' Efficacy is the level of reduction of target harmful organisms or damage caused by them to the plant,
#' after application of a treatment, compared to an untreated control.
#' Efficacy is expressed in percentage.
#'
#' @param value numeric value
#' @param value_tnt numeric value of TNT
#'
#' @returns
#'
#' @examples
#' @export
efficacy <- function(value,value_tnt)
{
  eff <- 100-((value*100/value_tnt))
  if(eff<0) {eff <- 0} ## to avoid negative value
  return(eff)
}

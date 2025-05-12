#' calcuate frequency in percent of values >0from a numeric vector
#'
#' @param vecteur
#'
#' @returns a numeric value of frequency, in percent
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 0, 5, 6, 0)
#' frequency(vec)
frequency <- function(vecteur) {
  # Check if the input is a numeric vector
  if (!is.numeric(vecteur)) {
    stop("Input must be a numeric vector.")
  }
  # Calculate the frequency of values greater than zero
  freq <- length(vecteur[vecteur > 0]) * 100 / length(vecteur)
  return(freq)
}
# TO DO check if NA values in vecteur ?

#' calcuate intensity (mean of value)
#'
#' @param vecteur
#'
#' @returns a numeric value of intensity
#' @export
#'
#' @examples
#' vec <- c(100, 20, 30, 0, 5, 0, 0)
#' intensity(vec)
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


#' calcul efficacy
#'
#' @param value numeric value
#' @param value_tnt numeric value of TNT
#'
#' @returns
#' @export
#'
#' @examples
efficacy <- function(value,value_tnt)
{
  eff <- 100-((value*100/value_tnt))
  return(eff)
}
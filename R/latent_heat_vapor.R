#' Calculate the Latent Heat of Vaporisation
#'
#' This function calculates the latent heat of vaporisation (\eqn{\lambda}) using
#' the Priestley and Taylor equation for a given temperature. The equation
#' takes air temperature (\eqn{tas}) in degrees Celsius as input and returns
#' the latent heat of vaporisation in MJ/kg. The equation is sourced from the
#' study by Priestley and Taylor (1972) which can be accessed [here](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2012GL053703#grl29749-bib-0021).
#'
#' @param tas Temperature in degrees Celsius. This is the air temperature at which
#'   the latent heat of vaporisation is to be calculated.
#'
#' @return Returns the latent heat of vaporisation (\eqn{\lambda}) in MJ/kg as
#'   a numeric value. The calculation is based on the Priestley and Taylor
#'   equation: \eqn{\lambda = \frac{2500.8 - 2.36 \times tas + 0.0016 \times tas^2 - 0.00006 \times tas^3}{1000}}.
#'
#' @examples
#' lambda(25) # Calculates the latent heat of vaporisation at 25 degrees Celsius
#'
#' @references
#' Priestley, C. H. B., and Taylor, R. J. (1972). On the assessment of surface
#' heat flux and evaporation using large-scale parameters. AGU Publications.
#' [DOI](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2012GL053703#grl29749-bib-0021)
#'
#' @export

latent_heat_vapor <- function(tas) {
  return((2500.8 - 2.36 * tas + 0.0016 * tas^2 - 0.00006 * tas^3) / 1000)
}

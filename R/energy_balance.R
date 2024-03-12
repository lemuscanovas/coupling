#' Calculate Energy Balance (H or Hp)
#'
#' This function calculates the energy balance in watts per square meter (W/m^2),
#' taking into account either actual (E) or potential (Ep) evaporation. The calculation
#' is based on the net radiation (Rn), the latent heat of vaporization (l), and
#' evaporation (E or Ep). The function adheres to the ECMWF convention that vertical
#' fluxes are positive downwards.
#'
#' @param Rn Net radiation at the surface in joules per square meter (J/m^2).
#' @param l Latent heat of vaporization in megajoules per kilogram (MJ/kg).
#' @param E_evap Evaporation (actual E or potential Ep) in millimeters (mm).
#'   This represents the water amount that evaporates or is potentially evaporated.
#' @param time_period The time period over which the energy is expended, in seconds.
#'   Default is 86400 (1 day).
#'
#' @return Returns the energy balance (H or Hp) in watts per square meter (W/m^2).
#'   The formula used for the calculation is:
#'   \eqn{H = \frac{Rn}{time\_period} - \left(\frac{l \times E \times 10^6}{time\_period}\right)},
#'   where \code{time_period} is 86400 seconds (1 day). The factor of \eqn{10^6}
#'   converts l from MJ/kg to J/kg for accurate computation.
#'
#' @details The function accounts for the conversion of the energy used for
#'   evaporation from joules to watts by dividing by the time period over which
#'   the energy is expended (1 day = 86400 seconds). This conversion is essential
#'   for representing the energy balance in terms of power (W/m^2).
#'
#' @examples
#' calculate_H(1000000, 2.45, 5) # Calculates H given Rn, l, and E values
#'
#' @note Caution: The ECMWF convention for vertical fluxes is positive downwards.
#'   This function conforms to this convention by adjusting the calculation of
#'   the energy balance accordingly.
#'
#' @export
energy_balance <- function(Rn, l, E) {
  time_period <- 86400  # 1 day in seconds
  return((Rn / time_period) - (l * E * 1000000 / time_period))
}

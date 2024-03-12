#' Calculate Lower Pi (π) for Day-by-Day Coupling Estimation
#'
#' This function calculates the Lower Pi (\(pi\)) to estimate the coupling between
#' energy balance components (H and Hp) and air temperature (tas) on a day-by-day basis.
#' Greater values of \(pi\) indicate stronger coupling. The function standardizes
#' the temperature (T), actual energy balance (H), and potential energy balance (Hp)
#' before calculating the product of the difference between standardized H and Hp
#' with standardized T. This metric aims to provide insights into the daily dynamics
#' of coupling.
#'
#' @param tas An SpatRaster time series of air temperature measurements in degrees Celsius.
#' @param H An SpatRaster time series representing the actual energy balance measurements
#'   in watts per square meter (W/m^2).
#' @param Hp An SpatRaster time series representing the potential energy balance
#'   measurements in watts per square meter (W/m^2).
#'
#' @return Returns An SpatRaster time series of \(pi\) values, representing the daily
#'   coupling between the energy balance components and air temperature. Higher values
#'   indicate greater coupling.
#'
#' @examples
#' # Assuming tas, H, and Hp are time series of temperature, actual energy balance,
#' # and potential energy balance, respectively:
#' tas <- ts(rnorm(365), start = c(2020, 1), frequency = 365)
#' H <- ts(rnorm(365), start = c(2020, 1), frequency = 365)
#' Hp <- ts(rnorm(365), start = c(2020, 1), frequency = 365)
#' daily_coupling <- calculate_p(tas, H, Hp)
#' plot(daily_coupling, type = 'l', main = "Daily Coupling Estimation")
#'
#' @details The function uses `app` to apply the `scale` function to each of the input
#'   series, standardizing them. It then calculates the difference between standardized
#'   H and Hp, multiplies this difference by the standardized temperature (T_prime),
#'   and assigns the original dates to the resulting series. This process yields a
#'   daily estimate of coupling.
#'
#' @note Ensure that `tas`, `H`, and `Hp` have the same length and time alignment
#'   for accurate calculation. The function implicitly relies on the `app` and `time`
#'   functions, which should handle time series data appropriately, adjusting and
#'   returning dates for the output series.
#'
#' @importFrom terra app
#'
#' @export
calculate_p <- function(tas, H, Hp) {
  dates <- time(tas)
  T_prime <- app(tas, scale)  # Standardize T
  H_prime <- app(H, scale)  # Standardize H
  Hp_prime <- app(Hp, scale)  # Standardize Hp
  pi <- (H_prime - Hp_prime) * T_prime
  time(pi) <- dates
  return(pi)
}

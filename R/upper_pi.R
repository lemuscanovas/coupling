#' Calculate Upper Pi (Π) for Estimating Long-term Coupling Evidences
#'
#' This function calculates the Upper Pi (\(Pi\)) to estimate long-term coupling
#' evidences between variables. It is used to assess the degree of coupling between
#' energy balance components (H and Hp) and air temperature (tas), where greater
#' values of Pi indicate greater coupling. The function computes the Pearson
#' correlation coefficients between the standard deviation series (sds) of H and
#' tas, and Hp and tas, respectively. The difference between these two correlation
#' coefficients is then returned as the measure of coupling.
#'
#' @param H An SpatRaster representing the actual energy balance
#'   measurements in watts per square meter (W/m^2).
#' @param Hp An SpatRaster representing the potential energy balance
#'   measurements in watts per square meter (W/m^2).
#' @param tas An SpatRaster representing the air temperature
#'   measurements in degrees Celsius.
#'
#' @return Returns a numeric value representing the difference between the Pearson
#'   correlation coefficients of H and tas, and Hp and tas. This difference is used
#'   as an indicator of coupling, with greater values indicating stronger coupling.
#'
#' @examples
#' H <- rnorm(100)  # Simulated actual energy balance data
#' Hp <- rnorm(100)  # Simulated potential energy balance data
#' tas <- rnorm(100)  # Simulated air temperature data
#' calculate_P(H, Hp, tas)  # Calculate Upper Pi
#'
#' @details The function internally uses two helper functions, `sds` and `lapp`,
#'   which are assumed to perform standard deviation series calculation and a
#'   custom application of a function over matrices, respectively. It calculates
#'   the Pearson correlation coefficient within each row for H against tas and
#'   Hp against tas using `cor` with the method "pearson" and handling missing
#'   values with "pairwise.complete.obs".
#'
#' @note It is important to ensure that H, Hp, and tas are aligned in terms of
#'   their dimensions and represent comparable time series or spatial data points.
#'   The function assumes that missing values are handled appropriately before
#'   passing to the correlation calculation.
#' @importFrom terra sds lapp rast
#'
#' @export
upper_pi <- function(H, Hp, tas) {
  Htas <- sds(H,tas)
  Hptas <- sds(Hp,tas)

  pearson_Htas <- lapp(Htas, \(x,y) {
    out <- rep(NA, nrow(x))
    for (i in 1:nrow(x)) {
      out[i] <- cor(x[i,], y[i,], "pearson", use = "pairwise.complete.obs")
    }
    out
  })

  pearson_Hptas <- lapp(Hptas, \(x,y) {
    out <- rep(NA, nrow(x))
    for (i in 1:nrow(x)) {
      out[i] <- cor(x[i,], y[i,], "pearson", use = "pairwise.complete.obs")
    }
    out
  })

  return(pearson_Htas - pearson_Hptas)
}

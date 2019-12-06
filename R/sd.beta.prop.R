#' sd.beta.prop
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@param std.dev within group standard deviation.
#'@usage sd.beta.prop(data, sizes, std.dev)
#' @export sd.beta.prop

sd.beta.prop <- function(data, sizes, std.dev)
{
  data <- as.vector(data)
  sd <- sqrt(var(data))
  return(sd)
}

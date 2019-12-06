#' sd.beta
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@param std.dev within group standard deviation.
#'@param ... further arguments are ignored.
#'@usage sd.beta.prop(data, sizes, std.dev, ...)
#' @export sd.beta
#' @aliases sd.beta.prop

sd.beta <- function(data, sizes, ...)
{
  data <- as.vector(data)
  sizes <- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  std.dev <- sqrt(pbar * (1 - pbar))
  return(std.dev)

}

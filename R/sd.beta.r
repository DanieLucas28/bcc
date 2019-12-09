#' sd.beta
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@param ... further arguments are ignored.
#'@usage sd.beta(data, sizes, ...)
#' @export sd.beta


sd.beta <- function(data, sizes, ...)
{
  data <- as.vector(data)
  sizes <- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  std.dev <- sqrt(pbar * (1 - pbar))
  return(std.dev)

}

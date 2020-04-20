#' sd.beta
#'
#' Performs the calculation of the standard deviation \code{std.dev} that will be used in the construction of the control chart.
#'@description These functions are used to compute statistics required by the beta chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@param ... further arguments are ignored.
#'@usage sd.beta(data, sizes, ...)
#'@export sd.beta
#'@return The function \code{sd.beta} returns \code{std.dev} the standard deviation of the statistic charted.
#'
#'@examples
#'data(Montgomery2005)
#'sd.beta(Montgomery2005$Defective, Montgomery2005$Sample)


sd.beta <- function(data, sizes, ...)
{
  data <- as.vector(data)
  sizes <- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  std.dev <- sqrt(pbar * (1 - pbar))
  return(std.dev)

}

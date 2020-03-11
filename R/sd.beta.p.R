#' sd.beta.p
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@param std.dev within group standard deviation.
#'@usage sd.beta.p(data, sizes, std.dev)
#'@export sd.beta.p
#'
#'@examples
#'data(montgomery2005)
#'sd.beta.p(Drapper1998data)


sd.beta.p <- function(data, sizes, std.dev)
{
  data <- as.vector(data)
  sd <- sqrt(var(data))
  return(sd)
}

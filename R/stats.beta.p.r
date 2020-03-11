#' stats.beta.p
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@usage stats.beta.p(data,sizes)
#'@export stats.beta.p
#'
#'@examples
#'data(Drapper1998data)
#'sd.beta.p(Drapper1998data)

stats.beta.p <- function(data, sizes)
{
  statistics <- as.vector(data)
  center <- mean(statistics)
  list(statistics = statistics, center = center)
}

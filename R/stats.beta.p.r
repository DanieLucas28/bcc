#' stats.beta.p
#'
#' Provides a list containing the center line of the graph \code{center} and the data to be used in the construction of the chart \code{statistics}.
#'@description These functions are used to compute statistics required by the beta chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@usage stats.beta.p(data,sizes)
#'@export stats.beta.p
#'@return The function \code{stats.beta.p} returns a list with components \code{statistics} and \code{center}.
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

#' stats.beta
#'
#' Provides a list containing the centerline of the chart \code{pbar} and the data manipulated to be used in the construction of the chart \code{data/sizes}.
#'@description These functions are used to compute statistics required by the beta chart.
#'@usage stats.beta(data, sizes)
#'@param data the observed data values.
#'@param sizes sample sizes
#'@export stats.beta
#'@return The function \code{stats.beta} returns a list with components \code{statistics} and \code{center}.
#'
#'@examples
#'data(Montgomery2005)
#'stats.beta(Montgomery2005$Defective, Montgomery2005$Sample)


stats.beta <- function(data, sizes)
{
  data <- as.vector(data)
  sizes<- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  list(statistics = data/sizes, center = pbar)
}

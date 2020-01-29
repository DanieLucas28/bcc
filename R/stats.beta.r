#' stats.beta
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@usage stats.beta(data, sizes)
#'@param data the observed data values.
#'@param sizes sample sizes
#'@export stats.beta

stats.beta <- function(data, sizes)
{
  data <- as.vector(data)
  sizes<- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  list(statistics = data/sizes, center = pbar)
}

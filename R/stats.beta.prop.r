#' stats.beta.prop
#'
#' modified qcc functions
#'@description These functions are used to compute statistics required by the p chart.
#'@param data the observed data values.
#'@param sizes sample sizes
#'@usage stats.beta.prop(data,sizes)
#' @export stats.beta.prop

stats.beta.prop <- function(data, sizes)
{
  statistics <- as.vector(data)
  center <- mean(statistics)
  list(statistics = statistics, center = center)
}

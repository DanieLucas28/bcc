#' stats.beta.prop
#'
#' modified qcc functions
#'
#'
#'
#' @export stats.beta.prop
#' @param data,sizes Numeric vector

stats.beta.prop <- function(data, sizes)
{
  statistics <- as.vector(data)
  center <- mean(statistics)
  list(statistics = statistics, center = center)
}

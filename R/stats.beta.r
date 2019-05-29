#' stats.beta
#'
#' modified qcc functions
#'
#'
#'
#' @export stats.beta

stats.beta <- function(data, sizes)
{
  data <- as.vector(data)
  sizes<- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  list(statistics = data/sizes, center = pbar)
}

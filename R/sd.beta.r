#' sd.beta
#'
#' modified qcc functions
#'
#'
#'
#' @export sd.beta
#' @param data,sizes Numeric vector

sd.beta <- function(data, sizes, ...)
{
  data <- as.vector(data)
  sizes <- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  std.dev <- sqrt(pbar * (1 - pbar))
  return(std.dev)

}

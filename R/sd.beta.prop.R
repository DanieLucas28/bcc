#' sd.beta.prop
#'
#' modified qcc functions
#'
#'
#'
#' @export sd.beta.prop
#' @param data,sizes Numeric vector
#' @param std.dev A number

sd.beta.prop <- function(data, sizes, std.dev)
{
  data <- as.vector(data)
  sd <- sqrt(var(data))
  return(sd)
}

#'@title Limits for continuos data
#'
#' modified qcc functions
#'@description These functions are used to compute the upper and lower control limits.
#'@param center sample/group center statistic.
#'@param std.dev within group standard deviation.
#'@param sizes sample sizes.
#'@param conf a numeric value used t o comput control limits, specifying the confidence level (if 0 < conf < 1)
#'@param ... further arguments are ignored.
#'@usage limits.beta.p(center, std.dev, sizes, conf, ...)
#'@export limits.beta.p
#'
#'@examples
#'limits.beta.p(center = 0.9989597, std.dev = 0.0009362578, conf = 0.9)


limits.beta.p <- function(center, std.dev, sizes, conf, ...)
{
  if (conf >= 1)
  {
    show("without application, determine another value for confidence.level")
  }
  else
  { if (conf > 0 & conf < 1)
  {
    probucl<-0.99865
    problcl<-0.00135
    alfa<-center*(((center*(1-center))/(std.dev^2))-1)
    beta<-(1-center)*(((center*(1-center))/(std.dev^2))-1)


    lcl <-qbeta(problcl, alfa, beta)
    ucl <-qbeta(probucl, alfa, beta)

  }
    else stop("invalid conf argument. See help.")
  }
  limits <- matrix(c(lcl, ucl), ncol = 2)
  rownames(limits) <- rep("", length = nrow(limits))
  colnames(limits) <- c("LCL", "UCL")
  return(limits)
}


#' limits.beta.prop
#'
#' modified qcc functions
#'
#'
#'
#' @export limits.beta.prop
#' @param center,std.dev,conf A number
#' @param sizes Numeric vector

limits.beta.prop <- function(center, std.dev, sizes, conf)
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


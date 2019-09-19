#' limits.beta
#'
#' modified qcc functions
#'
#'
#'
#' @export limits.beta

limits.beta <- function(center, std.dev, sizes, conf)
{
  probucl<-0.99865
  problcl<-0.00135
  sizes <- as.vector(sizes)
  n<-mean(sizes)
  alfa<-center*(((center*(1-center))/((center*(1-center))/n))-1) #=B5*(((B5*(1-B5))/((B5*(1-B5))/B4))-1)
  beta<-(1-center)*(((center*(1-center))/((center*(1-center))/n))-1) #=(1-B5)*(((B5*(1-B5))/((B5*(1-B5))/B4))-1)


  if (conf >= 1)
  {
    show("without application, determine another value for confidence.level")
  }
  else
  { if (conf > 0 & conf < 1)
  {
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

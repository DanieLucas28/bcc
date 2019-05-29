#' functions
#'
#' modified qcc functions
#'
#'
#'
#' @export stats.beta
#' @export sd.beta
#' @export limits.beta
#' @export stats.beta.prop
#' @export sd.beta.prop
#' @export limits.beta.prop

stats.beta <- function(data, sizes)
{
  data <- as.vector(data)
  sizes<- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  list(statistics = data/sizes, center = pbar)
}

sd.beta <- function(data, sizes, ...)
{
  data <- as.vector(data)
  sizes <- as.vector(sizes)
  pbar <- sum(data)/sum(sizes)
  std.dev <- sqrt(pbar * (1 - pbar))
  return(std.dev)

}

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

stats.beta.prop <- function(data, sizes)
{
  statistics <- as.vector(data)
  center <- mean(statistics)
  list(statistics = statistics, center = center)
}

sd.beta.prop <- function(data, sizes, std.dev)
{
  data <- as.vector(data)
  sd <- sqrt(var(data))
  return(sd)
}


limits.beta.prop <- function(center, std.dev, sizes, conf, ...)
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


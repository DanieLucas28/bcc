\name{Montgomery2005}
\alias{Montgomery2005}
\docType{data}

\title{Montgomery 2005 data}

\description{
Montgomery2005 consists of a data set of a manufacturing process offrozen orange juice concentrate in 30 packages of 50 units each.
}

\usage{data(Montgomery2005)}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{Sample}{a numeric vector}
    \item{Defective}{a numeric vector}
  }
}
%\details{}
%\source{}
\references{
Montgomery, D. C. (2005). \emph{Introduction to Statistical Quality Control(5th ed.)}, NewYork: John Wiley & Sons. 732p.}
\examples{
data(Montgomery2005)
attach(Montgomery2005)
boxplot(Montgomery2005)
plot(Montgomery2005)
}
\keyword{datasets}

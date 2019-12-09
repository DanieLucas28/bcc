\name{Drapper1998data}
\alias{Drapper1998data}
\docType{data}

\title{Drapper 1998 data}

\description{
Drapper1998data consists of a data set of the study of contaminated peanut by toxic substances in 34 batches of 120 pounds.
}

\usage{data(Drapper1998data)}
\format{
  A data frame with 34 observations on the following 1 variable.
  \describe{
    \item{PROP}{a numeric vector}
  }
}
%\details{}
%\source{}
\references{
Draper, N. R., & Smith, H. (1998) \emph{Applied Regression Analysis}, New York: John Wiley & Sons. 706p.}
\examples{
data(Drapper1998data)
attach(Drapper1998data)
boxplot(Drapper1998data)
plot(Drapper1998data)
}
\keyword{datasets}

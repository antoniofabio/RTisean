\name{logistic}
\alias{logistic}
\title{Logistic model }
\description{Creates a time series from the logistic model.
}
\usage{
logistic(iter = 1000, r = 4, x = 0.2, trans = 0)
}
\arguments{
  \item{iter}{length of the time series. }
  \item{r}{parameter of the logistic model. }
  \item{x}{initial cohordinate. }
   \item{trans}{number of transients discarded. }
}
\value{ A vector containing the time series. }
\examples{
dat <- logistic(10000,3.95,0.1,1000)
delayeddat <- delay(dat)
plot(delayeddat,xlab="x(t)",ylab="x(t+1)", main="Logistic Model Embedded Chaotic Attractor",cex=0.6,col=2)
}
\keyword{ ts }

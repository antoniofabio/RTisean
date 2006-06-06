\name{logistic}
\alias{logistic}
\title{Logistic model }
\description{
Creates a time series from the logistic model.
}
\usage{
logistic(iter=1000, r=4,x=0.2,t=0,noisesd=0)
}
\arguments{
  \item{iter}{length of the time series. }
  \item{r}{parameter of the logistic model. }
  \item{x}{initial cohordinate. }
   \item{t}{number of transients discarded. }
    \item{noisesd}{standard deviaton of white Gaussian noise. }
}
\value{ 
A vector containing the time series.
}
\details{If \code{noisesd} is set greater than zero,
then the iterated model becomes:

\eqn{x_[t+1]=rx_[t](1-x_[t])+n},

where \eqn{n} is drawn from \eqn{N(0,\code{noisesd})}.
}

\examples{
\dontrun{

dat <- logistic(150,3.9,0.1,1000)
plot(dat,xlab="Time",ylab="x", main="Time Series from Logistic Model with r=3.9",
type="l")

}
}

\keyword{ ts }
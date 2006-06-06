\name{notch}
\alias{notch}
\title{Notch filter}
\description{Notch filter in the time domain.}
\usage{notch(series, X, f = 1, w, l, x = 0, c = 1)}
\arguments{
  \item{series}{a matrix or a vector.}
  \item{X}{frequency to be cancelled. }
  \item{f}{sampling rate of data. }
  \item{w}{width of filter. }
  \item{l}{number of values to be read.  }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read. }
}
\value{A vector consisting of the filtered time series.}
\examples{
\dontrun{

numdata <- 500      
dat <- cos(1:numdata/25)+rnorm(numdata,0,0.1)
plot(dat,xlab="Time",t="l",ylab="Cos Data")
filteredat <- notch(dat,X=100)
points(filteredat,col=2,t="l")

dat <- lynx
plot(log(dat),ylab="Lynx data")
filteredat <- log(notch(dat,X=0.5,w=1))
filteredat <- ts(filteredat, start=1821, end=1934)
points(filteredat ,col=2,t="l")

}
}
\keyword{ts}
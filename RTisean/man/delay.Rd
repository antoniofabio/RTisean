\name{RT_delay}
\alias{RT_delay}
\title{Embed using delay coordinates}
\description{Calculates delay coordinates of a time series.}
\usage{
RT_delay(series, d = 1, m = 2, l, x = 0, c = 1)
}
\arguments{
\item{series}{a vector or a matrix.}
\item{d}{delay.}
\item{m}{embedding dimension.}
\item{l}{number of values to be read.}
\item{x}{number of lines to be ignored.}
\item{c}{column to be read.}
}

\value{A matrix containing the delayed time series.}
\examples{
\dontrun{

dat <- logistic(10000,4,0.1,1000)
delayeddat <- RT_delay(dat)
plot(delayeddat,xlab="x(t)",ylab="x(t+1)", main="Logistic Model Delayed Chaotic Attractor",cex=0.6,col=2)

}
}

\keyword{math}
\keyword{ts}

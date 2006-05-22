\name{RT_delay}
\alias{RT_delay}
\title{Embed using delay coordinates}
\description{Calculates delay cohordinates of a time series.}
\usage{
RT_delay(series, d = 1, m = 2, l = -1, x = 0, c = 1)
}
\arguments{
\item{series}{a vector or a matrix.}
\item{d}{delay.}
\item{m}{embedding dimension.}
\item{l}{number of values to be read.}
\item{x}{TO DO}
\item{c}{column to be read.}
}

\value{A matrix containing the delayed time series.}
\examples{
dat <- henon(10000)
delaycohord <- RT_delay(dat,d=2,l=1000)
}

\keyword{math}
\keyword{ts}


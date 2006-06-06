\name{timerev}
\alias{timerev}
\title{Time reversal asymmetry statistic}
\description{Computes the time reversal asymmetry statistic.}
\usage{
timerev(series, d = 1, l, x = 0, c = 1)
}
\arguments{
  \item{series}{a vector or a matrix.}
  \item{d}{delay. }
  \item{l}{number of points. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read. }
}
\value{
A scalar corresponding to the time reversal statistics.
}
\examples{
\dontrun{

dat <- lynx
trstat <- timerev(dat)

}
}
\keyword{math}
\keyword{ts}
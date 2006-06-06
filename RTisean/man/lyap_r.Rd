\name{lyap_r}
\alias{lyap_r}
\title{Largest Lyapunov exponent }
\description{Estimates the largest Lyapunov exponent of a given scalar data set.}
\usage{
lyap_r(series, l, x = 0, c = 1, m = 2, d = 1, t = 0, r, s = 50)
}
\arguments{
  \item{series}{a vector or a matrix.}
  \item{l}{number of data to use.}
  \item{x}{number of lines to be ignored.}
  \item{c}{column to be read.}
  \item{m}{embedding dimension.} 
  \item{d}{delay.}
  \item{t}{window around the reference point which should be omitted.}
  \item{r}{minimal length scale for the neighborhood search.}
  \item{s}{number of iterations in time.}
}
\value{A matrix containing the number of the iteration in the first column
and the logarithm of the stretching factor in the second column.}
\references{M. T. Rosenstein, J. J. Collins, C. J. De Luca, A practical method for calculating largest 
Lyapunov exponents from small data sets, Physica D 65, 117 (1993).}
\seealso{ \code{\link[tseriesChaos]{lyap_k}}}
\examples{
\dontrun{

dat <- logistic()
lyapoutput <- lyap_r(dat,s=200)
plot(lyapoutput,xlab="time",ylab="log(stretching factor)",type="l")

}
}
\keyword{ ts}
\name{pc}
\alias{pc}
\title{Embed using principal components}
\description{Writes the projections of a time series onto the largest principal components}
\usage{
pc(series, m, d = 1, q = 2, l, x = 0, c = 1)
}
\arguments{
  \item{series}{a vector or a matrix.}
  \item{m}{initial embedding dimension.}
  \item{d}{delay for initial embedding.  }
  \item{q}{number of principal components.}
  \item{l}{number of values to be skipped. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read.}
}
\value{A matrix containing the projections onto the largest principal components as columns.}
\seealso{ \code{\link{svd}}, \code{\link{princomp}}, \code{\link{prcomp}} }
\examples{
\dontrun{

dat <- henon(100)
prcomp <- pc(dat, m=1)
plot(prcomp,t="l",main="Projection on the first component",xlab="Time",ylab="Projected time series")

}
}
\keyword{manip}
\name{c1}
\alias{c1}
\title{Fixed mass estimation of information dimension}
\description{Computes curves for the fixed mass computation of the information dimension. }
\usage{
c1(series, d, m, M, t, n, scale = 2, K = 100, l, x = 0, c = 1,pretty = FALSE)
}

\arguments{
  \item{series}{a vector or matrix.}
  \item{d}{delay. }
  \item{m}{minimal embedding dimension. }
  \item{M}{maximal embedding dimension.}
  \item{t}{minimal time separation. }
  \item{n}{minimal number of center points. }
	\item{scale}{ resolution, values per octave (2) }
  \item{K}{maximal number of neighbours.}
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped. }
  \item{c}{column(s) to be read.}
  \item{pretty}{clean up output.}
}
\details{ The parameter \code{pretty} must be set to \code{FALSE}
if the output of \code{d2} is meant to be post-processed by
 \code{c2d} or \code{c2t}.
}
\value{A list of as many matrices as the embedding dimensions, containing 
the necessary radius in the first column and the mass in the second column.
}
\seealso{ \code{\link{d2}} }
\examples{
\dontrun{

dat <- henon(10000)
infodim <- c1(dat,d=1,m=2,M=6,t=50,n=500)

}
}
\keyword{ts}
\name{upo}
\alias{upo}
\title{Unstable periodic orbits}
\description{
Tools for finding unstable periodic orbits and their stability.
}
\usage{
upo(series, m = -1, r = -1, v = -1, p = 1, w = -1, W = -1, a = -1, s = -1, n = -1, l = -1, x = 0, c = 1,pretty=FALSE)
}
\arguments{
  \item{series}{a matrix or vector.}
  \item{m}{embedding dimension.}
  \item{r}{absolute kernel bandwidth.}
  \item{v}{same as fraction of standard deviation. }
  \item{p}{period of orbit.}
  \item{w}{minimal separation of trial points.}
  \item{W}{minimal separation of distinct orbits. }
  \item{a}{maximal error of orbit to be plotted.  }
  \item{s}{initial separation for stability.}
  \item{n}{number of trials. }
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read. }
  \item{pretty}{clean output up. }
}
\details{The parameter \code{pretty} must be set to \code{FALSE} if the output of \code{upo} is
meant to be post-processed by \code{upoembed}.}
\value{A list of three-column matrices, containing respectively the order of period,
the level of accuracy and the most unstable orbit eigenvalue.
}
\seealso{  \code{\link{upoembed}} }
\note{This actually gives erroneous results}
\examples{
\dontrun{

dat <- henon(1000)
noise <- rnorm(1000,0,0.1*sd(dat[,2]))
dat[,2] <- dat[,2]+ noise
unstperorbit <- upo(dat,m=2, v=0.1, n=70, p=6,pretty=T)

}
}
\keyword{ts}
\keyword{internal}
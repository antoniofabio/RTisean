\name{upo}
\alias{upo}
\title{Unstable periodic orbits}
\description{
Tools for finding unstable periodic orbits and their stability.
}
\usage{
upo(series, m = -1, r = -1, v = -1, p = 1, w = -1, W = -1, a = -1, s = -1, n = -1, l = -1, x = 0, c = 1)
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
}
\value{A list of lists, one for each orbit, containing the order of period,
the level of accuracy, the most unstable orbit eigenvalue and the orbit.
}
\seealso{  \code{\link{upoembed}} }
\examples{
dat <- henon(1000)
noise <- rnorm(1000,0,0.1*sd(dat[,2]))
dat[,2] <- dat[,2]+ noise
(unstperorbit <- upo(dat,m=2, v=0.1, n=70, p=6))
}
\keyword{ts}

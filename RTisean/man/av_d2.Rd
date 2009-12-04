\name{av_d2}
\alias{av_d2}
\title{Smoothing correlation sum data}
\description{
Takes the output of either \code{\link{d2}} or \code{\link{c1}}
and smoothes it by arithmetically averaging over a given interval.
}
\usage{
av_d2(lst, m = 1, M, a = 1, E = FALSE)
}
\arguments{
  \item{lst}{The output of \code{\link{d2}}, \code{\link{c1}}.}
  \item{m}{ minimal dimension to print.}
  \item{M}{maximal dimension to print.}
  \item{a}{ smooth over an interval of 2\code{a}+1 points.}
  \item{E}{ show the length scales in rescaled units.}
}
\value{A list of as many a matrices as the number of dimensions,
each containing the epsilon values in the first column and
the smoothed correlation sums in the second column.}
\seealso{ \code{\link{c2d}}, \code{\link{c2g}}, \code{\link{c2t}} .}
\examples{
\dontrun{


dat <- henon(l=10000,x=1000)
d2output <- d2(dat)
cordim <- d2output$.d2
smoothed <- av_d2(cordim)
plot(smoothed[[1]],t="l",ylim=c(0,5.5),col=2,xlab="Epsilon",
ylab=paste("Smoothed", expression(D[2](m,epsilon))),log="x", main="Smoothed 
Correlation Dimension Plot")
  for (a in 2:10) points(smoothed[[a]],t="l",col=2)


}
}
\keyword{math}
\keyword{ts}
\name{c2d}
\alias{c2d}
\title{Local slopes from correlation sums.}
\description{Reads the correlation integral, output of 
\code{\link{c1}} or \code{\link{d2}} and computes local slopes by fitting straight lines.
}
\usage{c2d(lst, a = 1)
}
\arguments{
  \item{lst}{ the output of  \code{c1} or \code{d2}.}
  \item{a}{ (-\code{a}, +\code{a}) is the range of the fits.}
}
\value{
A list of as many matrices as the number of dimensions of \code{lst}.
Each matrix contains the values of epsilon in the first column and
the local slopes in the second column.
}
\seealso{ \code{\link{c2t}}, \code{\link{c2g}} }
\examples{
\dontrun{

dat <- henon(l=10000,x=1000)
infodim <- c1(dat,d=1,m=2,M=6,t=50,n=500)
localslopes <- c2d(infodim, a=1)

plot(localslopes[[1]],t="l",ylim=c(0,3.5), xlim=c(0.001,2), col=2,xlab="Epsilon",
ylab="Local Slopes",log="x", main="Local Slope Plot, embedding dims=(2,...,6)")
for (a in 2:5) 
	lines(localslopes[[a]],col=2)

}
}
\keyword{ts}
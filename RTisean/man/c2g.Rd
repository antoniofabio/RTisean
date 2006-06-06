\name{c2g}
\alias{c2g}
\title{Gaussian kernel correlation integral}
\description{Reads the correlation integral, output of  \code{d2}
and computes the Gaussian kernel correlation integral.}
\usage{
c2g(lst)
}
\arguments{
  \item{lst}{ the output of  or \code{d2}.  }
}
\value{ A list of as many matrices as the number of dimensions of \code{lst}.
Each matrix contains the values of epsilon in the first column,
the Gaussian kernel correlation integral in the second column
and its logarithmic derivative in the third columns.
}
\references{J. M. Ghez and S. Vaienti, Integrated wavelets on fractal sets I: 
The correlation dimension, Nonlinearity 5, 777 (1992).  }
\seealso{\code{\link{c2d}}, \code{\link{c2t}}}
\examples{
\dontrun{
 
dat <- henon(10000)
d2output <- d2(dat)
corsum <- d2output$.c2
corint <- c2g(corsum)
plot(corint[[1]],t="l",ylim=c(0,1),col=2,xlab="Epsilon",
ylab="Correlation Integral",log="x", main="Correlation Integral Plot, embedding dims = (1,...,10)")

for (a in 2:10) 
	lines(corint[[a]],col=2)

}
}
\keyword{ ts }
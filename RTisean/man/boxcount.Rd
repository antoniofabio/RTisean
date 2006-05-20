\name{boxcount}
\alias{boxcount}
\title{ Renyi entropy estimate }
\description{Estimates the Renyi entropy using a partition of the phase space. }
\usage{
boxcount(series, l = -1, x = 0, c = -1, d = 1, M = -1, Q = 2, R = -1, r = -1, scale = 20)
}
\arguments{ \item{series}{ a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read.  }
  \item{d}{delay for the delay vectors. }
  \item{M}{number of components, maximal embedding dimension. }
  \item{Q}{order of the entropy. }
  \item{R}{maximal length scale. }
  \item{r}{minimal length scale. }
  \item{scale}{number of epsilon values. }
}
\details{This function also can handle multivariate data, so that the phase space 
is built of the components of the time series plus a temporal embedding, if desired.}

\value{
A list containing as many lists as the number of components,
each containing as many matrices as the number of dimensions.
Each matrix contains: the value of epsilon
in the first column, the \code{Q}th order entropy (H\_Q(dimension,epsilon))
in the second column and the \code{Q}th order differential entropy
( H\_Q(dimension,epsilon)-H\_Q(dimension-1,epsilon)) in the third column.
}
\references{ }
\author{ }
\note{ }
\seealso{ \code{\link{d2}}, \code{\link{c1}} }
\examples{

dat <- henon(10000)
boxout<- boxcount(dat,d=2)
plot(boxout$comp1$embdim1[,1],boxout$comp1$embdim1[,2],ylim=c(0,8), t="l",
xlab="Epsilon",ylab="Entropy",main="Renyi Entropy of H�on Data")
lines(boxout$comp1$embdim2[,1],boxout$comp1$embdim2[,2],t="l",col=2)
legend(1.5,7, "Embedding dimension 1",fill=1, bty="n")
legend(1.5,6.7,"Embedding dimension 2",fill=2,bty="n")

  }
}
\keyword{ts} 

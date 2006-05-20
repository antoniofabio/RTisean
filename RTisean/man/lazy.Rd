\name{lazy}
\alias{lazy}
\alias{nrlazy}
\title{Nonlinear noise reduction}
\description{Tools for performing nonlinear noise reduction.}
\usage{nrlazy(series, l = -1, x = 0, c = 1, m = 5, d = 1, i = 1, r = -1, v = -1)
lazy(series, m = -1, r = -1, v = -1, i = 1, l = -1, x = 0, c = 1)
 }
\arguments{
  \item{series}{a vector or a matrix.}
  \item{m}{embedding dimension. }
  \item{d}{delay for the embedding. }
  \item{r}{neighborhood size.  }
  \item{v}{neighborhood size as fraction of data standard deviation.}
  \item{i}{number of iterations.}
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped.}
  \item{c}{column to be read. }
}

\details{In \code{nrlazy} each embedded point is replaced by the average vector 
calculated in its neighborhood with a given size.
In \code{lazy} only the central component of each vector is corrected. }

\value{A vector containing the filtered time series.}

\references{T. Schreiber, Extremely simple nonlinear noise reduction method, Phys. Rev. E 47, 2401 (1993)}
\author{  }
\note{ }
\seealso{\code{\link{ghkss}},\code{\link{project}}}

\examples{

\dontrun{
x11(12,6)
par(mfrow=c(1,2))

n=5000
dat <- henon(n)
dat[,1] <- dat[,1] + rnorm(n)/25
delayeddat <- delay(dat)
filtereddat <- lazy(dat,m=5,v=0.05,c=2)
delayedfiltdat <- delay(filtereddat)
plot(delayeddat, cex= 0.2, cex.main=0.7, xlab="", ylab="", main = "Embedded noisy H�on attractor filtered with lazy")
points(delayedfiltdat, cex=0.2, col=2)
legend(-0.25,-0.5, "Noisy data",fill=1, bty="n",cex=0.7)
legend(-0.25,-0.75, "Filtered data",fill=2,bty="n",cex=0.7)


filteredat2 <- nrlazy(dat,m=5,v=0.2)
delayedfiltdat2 <- delay(filteredat2)
plot(delayeddat, cex= 0.2, cex.main=0.7,xlab="", ylab="", main =  "Embedded noisy H�on attractor filtered with nrlazy")
points(delayedfiltdat2 , cex=0.2, col=2)
legend(-0.25,-0.5, "Noisy data",fill=1,bty="n",cex=0.7)
legend(-0.25,-0.75, "Filtered data",fill=2,bty="n",cex=0.7)
}
}
\keyword{ ts } 

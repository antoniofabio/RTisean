\name{polynom}
\alias{polynom}
\title{Modeling data trough a polynomial ansatz}
\description{
This programs models a time series making a polynomial ansatz.}
\usage{
polynom(series, l, x = 0, c = 1, m = 2, d = 1, p = 2, n, L)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read. }
  \item{m}{embedding dimension. } 
  \item{d}{delay. }
  \item{p}{order of the polynomial. }
  \item{n}{number of points for the fit.} 
  \item{L}{length of the predicted series. }
}
\details{The points following the first \code{n} ones are used to estimate the out of sample error.}
\value{a list composed by:

  \item{coeff }{the model coefficients}
  \item{err }{the average insample error and average out of sample error, the latter if \code{L} is set >0.}
  \item{pred}{the predicted points, if \code{L} is set >0.}
  
}
\seealso{ \code{\link{polyback}} }
\examples{
\dontrun{


dat <- sin(1:10000)+rnorm(10000)/5
polynomout <- polynom(dat, p = 3, L=100, n=9900)
pred <- polynomout$pred
plot(dat[9901:10000],t="l",xlab="Time",ylab="Sin data",ylim=c(-1.9,max(dat)))
lines(pred,col=2)
legend(70,-1.5, "Noisy Data",fill=1, bty="n")
legend(70,-1.7,"Filtered Data",fill=2,bty="n")


}
}

\keyword{ models} 
\keyword{ ts}
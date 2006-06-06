\name{polynomp}
\alias{polynomp}
\title{Modeling data trough a polynomial ansatz}
\description{This programs models the data making a polynomial ansatz, reading the terms 
of the polynomial from a parameter matrix. }
\usage{polynomp(series, l, x = 0, c = 1, m = 2, d = 1, n, L = 1000, p)}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read.  }
  \item{m}{embedding dimension. }
  \item{d}{delay.}
  \item{n}{length for the insample error estimation.}
  \item{L}{length of the trajectory to forecast. }
  \item{p}{name of the input parameter matrix created with \code{polypar} or by hand.}
}
\value{
A list with components:

  \item{coeff }{the coefficients of the model.}
  \item{pred }{the forecasted points.}
}
\examples{
\dontrun{

dat<-sin(1:10000)+rnorm(10000)/5
polynompout<-polynomp(dat[1:9900], p = polypar(), L=100)
pred<-polynompout$pred
plot(dat[9901:10000],t="l",xlab="Time",ylab="Sin data",ylim=c(-1.9,max(dat)))
lines(pred,col=2)
legend(70,-1.5, c("Noisy Data", "Filtered Data") ,fill=c(1,2), bty="n")

}
}
\keyword{ models} 
\keyword{ ts }
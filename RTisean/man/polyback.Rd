\name{polyback}
\alias{polyback}
\title{Backward elimination for a given polynomial}
\description{Performs a backward elimination for a given polynomial, whose terms are 
read from a parameter matrix. The terms are removed in such a way that the one step 
forecast error is increased minimally. }
\usage{polyback(series, l, x = 0, c = 1, m = 2, d = 1, n, s = 1, scale = 1, p)}
\arguments{
  \item{series}{a vector or a matrix.}
  \item{l}{number of data to use.}
  \item{x}{number of lines to be ignored.}
  \item{c}{column to be read.  }
  \item{m}{embedding dimension.  }
  \item{d}{delay. }
  \item{n}{length for the insample error estimation.}
  \item{s}{steps to be forecasted. }
  \item{scale}{final number of terms.}
  \item{p}{name of the input matrix created with \code{polypar} or by hand.}
}
\value{
A dataframe containing the  number of remaining terms in the polynomial
in column 1, the in-sample error produced by the reduced polynomial in 
column 2, the out-of-sample error produced by the reduced polynomial in
column 3 and the term removed last from the polynomial in columns 4 and 5.
}
\seealso{ \code{\link{polypar}}}
\examples{
\dontrun{
 
polymat <- polypar(p=4)
polyout <- polyback(sunspot.month,p=polymat,n=2000)
plot(0:14,polyout[,2],t="l",ylim=range(polyout[,(2:3)]),
xlab="Number of removed parameters",ylab="Forecast error",
main="Fitting accuracy of polynomial
model reduced via backward elimination")
lines(0:14,polyout[,3],col=2)
legend(2,0.5, c("In-sample error","Out-of-sample error"),fill=c(1,2),bty="n",cex=0.8)

}
}
\keyword{ models} 
\keyword{ ts } 
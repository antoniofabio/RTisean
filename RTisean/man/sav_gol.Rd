\name{sav_gol}
\alias{sav_gol}
\title{Savitzky-Golay filter}
\description{
A Savitzky-Golay filter to either clean the data from high frequency noise or to get
 a better estimate of its derivative of a chosen order.}
\usage{
sav_gol(series, l, x = 0, c, m, n = "2,2", p = 2, D = 0)
}
\arguments{
  \item{series}{a vector or a matrix.}
  \item{l}{number of data to use.}
  \item{x}{number of lines to be ignored.}
  \item{c}{column to be read.}  
  \item{m}{number of components to be read (dimension).}
  \item{n}{a string containing the two lengths of the averaging windows back and forward in time, separated by comma (see example)}
  \item{p}{order of the fitted polynomial.}
  \item{D}{order of the derivative to be estimated.}
}
\value{
A matrix containining the filtered data, disposed in \code{l} lines, each of which has \code{m} columns.  
The first length of the averaging window back in time and the last length of the 
averaging window forward in time lines are special. They contain the raw data 
in the case that \code{D} was set to 0 and zeroes in the case that \code{D} was larger than zero. 
}
\references{
W. H. Press, B. P. Flannery, S. A. Teukolsky, and W. T. Vetterling, ``Numerical Recipes'', 2nd edn., Cambridge University Press, Cambridge (1992). 
 }
\examples{
\dontrun{

numdata <- 500
dat <- cos(1:numdata/25)+rnorm(numdata,0,0.1)
windowlength <- 15
plot(dat[windowlength:(numdata-windowlength)],xlab="Time",t="l",ylab="Cos Data",ylim=c(-1.5,1.2))
filteredata <- sav_gol(dat,n="15,15")
lines(filteredata[windowlength:(numdata-windowlength)],col=2,lwd=2)
legend(300,-1.2, c("Noisy Data","Filtered Data"),fill=c(1,2), bty="n")

}
}
\keyword{ts}
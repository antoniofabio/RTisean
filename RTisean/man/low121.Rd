\name{low121}
\alias{low121}
\title{Low pass filter }
\description{Applies a simple low pass filter in the time domain.}
\usage{low121(series, l = -1, x = 0, c = 1, i = 1)}
\arguments{
\item{series}{a vector or matrix.}
\item{l}{number of data to use. }
\item{x}{number of lines to be ignored. }
\item{c}{column to be read. }  
\item{i}{number of iterations. }
}
\value{A vector consisting of the filtered time series.}
\examples{
numdata <- 500
dat <- cos(1:numdata/25)+rnorm(numdata,0,0.1)
plot(dat,xlab="Time",t="l",ylab="Cos Data",ylim=c(-1.7,1.2))
filteredat <- low121(dat,i=10)
points(filteredat,col=2,t="l",lwd=2)
legend(300,-1.3, c("Noisy Data","Filtered Data"),fill=c(1,2), bty="n")
}
\keyword{ts}

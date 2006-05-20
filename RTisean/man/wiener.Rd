\name{wiener1}
\alias{wiener1}
\alias{wiener2}
\title{Wiener filter}
\description{Tools for producing the periodogram from a time series and generating a filtered sequence.}
\usage{
wiener1(series, f = -1, w = -1, l = -1, x = 0, c = 1)
wiener2(series, f = -1, w = -1, o = -1, l = -1, x = 0, c = 1)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{f}{sampling rate. }
  \item{w}{frequency resolution.}
  \item{o}{the output of a \code{wiener1} call.}
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read. }
}
\value{\code{wiener1} produces the periodogram matrix, \code{wiener2} generates the filtered time series.}
\examples{
numdata <- 500 
dat <- cos(1:numdata/25)+rnorm(numdata,0,0.1)
periodogramat<- wiener1(dat)
filteredat <- wiener2(dat, o=periodogramat)
plot(dat,xlab="Time",t="l",ylab="Cos Data",ylim=c(-1.5,1.2))
points(filteredat,col=2,t="l",lwd=2)
legend(300,-1.2, c("Noisy Data","Filtered Data") ,fill=c(1,2), bty="n")
}
}
\keyword{ts}

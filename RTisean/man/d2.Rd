\name{d2}
\alias{d2}
\title{Dimension and entropy estimation}
\description{Estimates the correlation sum, the correlation dimension and the correlation
entropy of a given, possibly multivariate, time series.}
\usage{
d2(series, l, x = 0, d = 1, M, c, t = 0, R, r, scale = 100, N = 1000, E = FALSE, pretty=FALSE)
}
\arguments{
  \item{series}{a vector or matrix.}
  \item{l}{number of data points to be used. }
  \item{x}{number of lines to be ignored.}
  \item{d}{delay for the delay vectors. }
  \item{M}{number of components, maximal embedding dimension} 
  \item{c}{columns to be read. }
  \item{t}{theiler window. }
  \item{R}{maximal length scale.  }
  \item{r}{minimal length scale. }
  \item{scale}{number of epsilon values. }
  \item{N}{maximal number of pairs to be used. }
  \item{E}{use data that is normalized to [0,1] for all components.}
	\item{pretty}{clean ouput for pretty printing}
}
\details{The parameter \code{pretty} must be set to \code{FALSE} if the output of \code{d2} is
meant to be post-processed by \code{av_d2}, \code{c2d},\code{cdg} or \code{c2t}.
}
\value{
A list of lists, each composed by as many matrices as the treated length scales and embedding dimensions.
The first column of each matrix contains the values of epsilon; the second column contains,
according to the list item:

\item{.c2 }{the correlation sums. }
\item{.d2 }{the local slopes of the logarithm of the correlation sum, the correlation dimension. }
\item{.h2 }{the correlation entropies.}

}

\seealso{ 
\code{\link{c1}}
}
\examples{
\dontrun{

dat <- henon(10000)
d2output <- d2(dat, pretty=TRUE)
cordim <- d2output$.d2
plot(cordim[[1]],t="l",ylim=c(0,7),col=2,xlab="Epsilon",
ylab=expression(D[2](m,epsilon)),log="x", main="Correlation Dimension Plot")
for (a in 2:10) 
	lines(cordim[[a]],col=2)

}
}
\keyword{ ts}
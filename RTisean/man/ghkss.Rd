\name{ghkss}
\alias{ghkss}
\title{Noise reduction}
\description{ 
Performs a noise reduction, through an orthogonal projection onto manifold using an 
euclidean or a special metric. }

\usage{
ghkss(series, l = -1, x = 0, c = 1, m = 5, d = 1, q = 3, k = 30, r = -1, i = 1, two = FALSE)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored.}
  \item{c}{column to be read. }
  \item{m}{embedding dimension. } 
  \item{d}{delay for the embedding. }
  \item{q}{dimension of the manifold to project to. }
  \item{k}{minimal number of neighbours. }
  \item{r}{minimal size of the neighbourhood. }
  \item{i}{number of iterations.}
  \item{two}{use euclidean metric instead of the special one.}
}
\details{
}
\value{A vector containing the filtered time series. }
\references{ P. Grassberger, R. Hegger, H. Kantz, C. Schaffrath, and T. Schreiber, On noise
 reduction methods for chaotic data, CHAOS 3, 127 (1993).}

\author{ }
\note{ }
\seealso{  \code{\link{project}} }
\examples{

dat <- lynx + 500*rnorm(114)
dat[dat<0] <- 0
plot(dat,ylab="Lynx data")
filteredat <- ghkss(dat)
filteredat <- ts(filteredat, start=1821, end=1934)
points(filteredat ,col=2,t="l")

}
\keyword{ts} 

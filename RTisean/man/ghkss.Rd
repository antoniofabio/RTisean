\name{ghkss}
\alias{ghkss}
\title{Noise reduction}
\description{ 
Performs a noise reduction, through an orthogonal projection onto manifold using an 
euclidean or a special metric. }
\usage{
ghkss(series, l, x = 0, c = 1, m = 5, d = 1, q = 3, k = 30, r, i = 1, two = FALSE)
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
\value{A list of vectors with filtered time series, one per iteration.}
\references{ P. Grassberger, R. Hegger, H. Kantz, C. Schaffrath, and T. Schreiber, On noise
 reduction methods for chaotic data, CHAOS 3, 127 (1993).}
\seealso{  \code{\link{project}} }
\examples{
\dontrun{

#FIXME: find better example
dat <- logistic(iter=10000,r=3.8, noisesd=0.01)
filteredat <- ghkss(dat)[[1]]
delayedfiltered <- embed(filteredat, d=2)
delayed <- embed(dat,d=2)
plot(delayed,cex=0.8,xlab="x(t)",ylab="x(t+1)",main="Delayed Logistic Chaotic Attractor",pch='.')
points(delayedfiltered,col=2,cex=0.8,pch='.')
legend(0.5,0.4, c("Noisy data","Filtered data"),fill=c(1,2), bty="n",cex=0.8)

}
}
\keyword{ts} 
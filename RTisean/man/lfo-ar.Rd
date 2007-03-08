\name{lfo.ar}
\alias{ll_ar}
\alias{lfo.ar}
\alias{lfo-ar}
\title{Modeling data through a local linear ansatz}
\description{Makes a local linear ansatz and estimates the one step 
prediction error of the model.}
\usage{
lfo.ar(series, l, x = 0, c = 1, m = c(1,2), d = 1, i, r, R, f = 1.2, s = 1, C)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of points to use.  }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read.  }
  \item{m}{no. of components, embedding dimension}
  \item{d}{delay for the embedding. }
  \item{i}{number of points for which the error should be calculated. }
  \item{r}{neighborhood size to start with. }
  \item{R}{neighborhood size to end with. }
  \item{f}{factor to increase the neighborhood size if not enough neighbors were found.}
  \item{s}{steps to be forecasted.}
  \item{C}{width of causality window.}
}
\value{
A matrix containing: the neighborhood size in column 1; the relative forecast error in columnn 2;
the fraction of points for which neighbors were found for the corresponding neighborhood size in column 3;
the average number of neighbors found per point in column 4; the variance of the fraction
of points for which neighbors were found in column 5.
}
\references{M. Casdagli, Chaos and deterministic versus stochastic nonlinear modeling, J. Roy. 
Stat. Soc. 54, 303 (1991).}
\seealso{ \code{\link{onestep}}}
\examples{
\dontrun{

dat <- logistic(iter=1000,r=3.6) +runif(1000)/10
ll_aroutput <- lfo.ar(dat)
par(mfrow=c(2,2))

plot(ll_aroutput[,1],ll_aroutput[,2],xlab="Neighborhood size",ylab="Relative forecast error",t="l")
plot(ll_aroutput[,1],ll_aroutput[,3],xlab="Neighborhood size", ylab="Fraction of points with neighbors",t="l" )
plot(ll_aroutput[,1],ll_aroutput[,4],xlab="Neighborhood size",ylab="Average number of neighbors",t="l")
plot(ll_aroutput[,1],ll_aroutput[,5],xlab="Neighborhood size",ylab="Variance of points with neighbors",t="l")

}
}
\keyword{ models}
\keyword{ ts}

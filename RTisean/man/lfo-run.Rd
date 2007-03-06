\name{lfo.run}
\alias{nstep}
\alias{lfo.run}
\title{Modeling data through a local linear ansatz}
\description{Makes either a local linear ansatz or a zeroth order ansatz 
for a time series and iterates an artificial trajectory. The initial values for the
trajectory are the last points of the original time series. Thus it actually 
forecasts the time series.  
}
\usage{
lfo.run(series, l, x = 0, m, c, d = 1, L = 1000, k = 30, r, f = 1.2, O = FALSE)
}
\arguments{
  \item{series}{a vector or matrix.}
  \item{l}{number of points to use. }
  \item{x}{number of lines to be ignored. }
  \item{m}{number of components, embedding dimension. }
  \item{c}{column to be read. }
  \item{d}{delay for the embedding. }
  \item{L}{length of prediction. }
  \item{k}{minimal numbers of neighbors for the fit. }
  \item{r}{neighborhood size to start with. }
  \item{f}{factor to increase the neighborhood size if not enough neighbors were found. }
  \item{O}{performs a zeroth order fit instead of a local linear one. }
}
\details{Once in an iteration the algorithm creates a point which is far away from the original time series,
 the procedure stops since no neighbors can be found and no local
model can be constructed (from Kantz, pag. 330).}
\value{A vector containing the forecasted time series.}
\examples{
\dontrun{

par(mfrow=c(2,1))
dat<-logistic(r=3.6)
nsteppred<-lfo.run(dat[1:950])
plot(dat[951:1000],t="l",xlab="Time",ylab="x",ylim=c(0.2,0.9),
main="Local linear model on logistic data")
lines(nsteppred,col=2)
validiter<-length(nsteppred)

legend(40,0.35, "Real data",fill=1,bty="n",cex=0.7)
legend(40,0.3, "Artificial data",fill=2,bty="n",cex=0.7)

par(cex.lab=0.8)
plot(abs(dat[951:1000][1:validiter]-nsteppred),t="l",xlim=c(1,50),xlab="Time",ylab="Distance from original time series")
text(40,1,paste("Last neighbor found at iteration", validiter),cex=0.7)

}
}
\keyword{ models}
\keyword{ ts}

\name{rbf}
\alias{rbf}
\title{Modeling data using a radial basis function ansatz}
\description{
Models data using a radial basis function ansatz. The basis functions used are 
Gaussians, with center points chosen to be data from the time series.
A kind of Coulomb force can be applied to them to let 
them drift a bit in order to distribute them more uniformly. The variance 
of the Gaussians is set to the average distance between the centers.
This fuction either tests the ansatz by calculating the average forecast error of the model
or makes a prediction.}
\usage{
rbf(series, l = -1, x = 0, c = 1, m = 2, d = 1, p = 10, X = FALSE, s = 1, n = -1, L = -1)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read.  }
  \item{m}{embedding dimension. }
  \item{d}{delay.}
  \item{p}{number of centers.}
  \item{X}{deactivate drift (Coulomb force). }
  \item{s}{steps to forecast.}
  \item{n}{number of points for the fit.}
  \item{L}{length of the predicted series. }
}
\details{The ansatz made is:
\deqn{x_{t+1}=a_0+ \sum_{a_i}{\phi(x_t)}}
where \eqn{x_t} is the \eqn{t}th delay vector and \eqn{phi} is a Gaussian centered at
the \eqn{i}th center point.
}
\value{
A list composed by:

\item{centers}{The matrix of the cohordinates of the center points.}
\item{variance}{The variance used for the Gaussians.}
\item{coeff}{The coefficients (weights) of the basis functions used for the model.}
\item{error}{The in-sample prediction error.}
\item{pred}{The predicted points, if the \code{L} parameter was set >0.}
}

\examples{
dat <- logistic(iter=10000,r=3.8, noisesd=0.015)
filteredat <- rbf(dat,L=1000,n=9000)
pred <- filteredat$pred
delayedfiltered <- delay(pred)
delayed <- delay(dat[9000:10000])
plot(delayed,cex=0.1,xlab="x(t)",ylab="x(t+1)",main="Delayed Logistic Chaotic Attractor")
points(delayedfiltered,col=2,cex=0.1)
legend(0.5,c("Noisy data","Filtered data"),fill=c(1,2), bty="n",cex=0.8)
}
\keyword{ ts } 


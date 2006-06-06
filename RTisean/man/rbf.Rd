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
rbf(series, l, x = 0, c = 1, m = 2, d = 1, p = 10, X = FALSE, s = 1, n, L)
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
\dontrun{

dat<-sin(1:10000)+rnorm(10000)/5
rbfout<-rbf(dat,L=100,n=9900)
pred<-rbfout$pred
plot(dat[9901:10000],t="l",xlab="Time",ylab="Sin data")
lines(pred,col=2)

}
}
\keyword{ ts } 

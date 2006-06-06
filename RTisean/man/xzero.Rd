\name{xzero}
\alias{xzero}
\title{Zeroth order model }
\description{
Takes two data sets and fits a zeroth order model of one data set  
to predict another data set. It then computes the error of the model.
}
\usage{
xzero(series, l, x = 0, c, m = 3, d = 1, n, k = 30, r, f = 1.2, s = 1)
}
\arguments{
  \item{series}{a matrix containing the two time series. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read. }
  \item{m}{embedding dimension. } 
  \item{d}{delay for the embedding. }
  \item{n}{number of points for which the error should be calculated.  }
  \item{k}{minimal numbers of neighbors for the fit. }
  \item{r}{neighborhood size to start with.  }
  \item{f}{factor to increase the neighborhood size if not enough neighbors were found.}
  \item{s}{steps to be forecasted.}
}
\value{
The output consists of a matrix with \code{s} rows, containing the steps forecasted in first column
and the relative forecast error in the second column. Relative means that the forecast error is divided
by the standard deviation of the second (the one which is forecasted) data set. 
}
\examples{
\dontrun{

dat <- as.vector(sunspot.month)
dat<- cbind(dat, dat + 10*rnorm(2988))
xzeroo <- xzero(dat,s=100)
plot(xzeroo,t="l",xlab="Time steps",ylab="Relative Error",
main="Cross prediction accuracy of a zeroth order model")

}
}
\keyword{ ts }
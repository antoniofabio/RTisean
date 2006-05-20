\name{svd}
\alias{svd}
\title{SVD}
\description{Performs a global SVD. }
\usage{
svd(series,l=-1,x=0,c=1,m=2,d=1,q=-1)

}
\arguments{
  \item{series}{a vector or a matrix.}
  \item{l}{number of data to be used. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read. }
  \item{m}{dimension of the basis}
  \item{d}{delay.}
  \item{q}{dimensions to write the time series down to.}
}
\details{}
\value{ A list composed by:

  \item{eigen}{a vector containing the singular values of the covariance matrix.}
  \item{project}{ the data in the basis of the eigenvectors if \code{q} is 
  smaller than \code{m}; the projected scalar time series otherwise. }
  }
\references{ }
\author{ }
\note{ }
\seealso{ \code{\link{pc}}} 
\examples{

dat=logistic()
svdout=svd(dat,q=2)
plot(svdout$project)

}
\keyword{ math }
\keyword{ manip}
\keyword{ ts}

\name{RT_svd}
\alias{RT_svd}
\title{SVD}
\description{Performs a global SVD. }
\usage{
RT_svd(series,l,x=0,c=1,m=2,d=1,q)
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
\value{ A list composed by:
  \item{eigen}{a vector containing the singular values of the covariance matrix.}
  \item{project}{ the data in the basis of the eigenvectors if \code{q} is 
  smaller than \code{m}; the projected scalar time series otherwise. }
}
\seealso{ \code{\link{pc}}} 
\examples{
\dontrun{

dat<-henon(100)
svdout<-RT_svd(dat,q=1)
plot(svdout$project,t="l",xlab="Time",ylab="Projected Time series")

}
}
\keyword{ math }
\keyword{ manip}
\keyword{ ts}
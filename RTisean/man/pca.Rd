\name{RT_pca}
\alias{RT_svd} %old routine name
\alias{RT_pca}
\title{PCA}
\description{Performs a global SVD. }
\usage{
RT_pca(series,l,x=0,c=1,m=c(1,2),d=1, W=0, q)
}
\arguments{
  \item{series}{a vector or a matrix}
  \item{l}{number of data to be used }
  \item{x}{number of lines to be ignored }
  \item{c}{column to be read }
  \item{m}{no. of input columns, embedding dimension}
  \item{d}{delay}
  \item{W}{an integer code between 0 and 3 indicating the kind of output to be produced (see \code{value} section)}
  \item{q}{meaning depends on \code{W}.
\code{W=2}: Number of components written.
\code{W=3}: Projection dimensiondimensions to write the time series down to}
}
\value{ Depends on the \code{W} option.
	\item{0}{the vector of eigenvalues}
	\item{1}{matrix of eigenvectors. The columns of the output matrix are the eigenvectors}
	\item{2}{Transformation of the time series onto the eigenvector basis. The number of components printed is determined by the \code{q} option}
	\item{3}{Projection of the time series onto the first \code{q} eigenvectors (global noise reduction)}
}
\seealso{ \code{\link{pc}}} 
\examples{
\dontrun{
dat<-henon(100)
svdout<-RT_pca(dat, W=3, q=1)
plot(svdout,t="l",xlab="Time",ylab="Projected Time series")

}
}
\keyword{ math }
\keyword{ manip}
\keyword{ ts}

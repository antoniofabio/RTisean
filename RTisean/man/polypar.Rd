\name{polypar}
\alias{polypar}
\title{Polynomial parameter matrix}
\description{Creates a matrix which can be used as input by the
 functions \code{polynomp} and \code{polyback}.}
\usage{
polypar(m = 2, p = 3)
}
\arguments{
  \item{m}{dimension of the polynomial. }
  \item{p}{order of the polynomial.}
}
\value{A matrix, each line of which looks like: \eqn{i_1, i_2, ..., i_d}. 
These \eqn{i}'s define the order of the delay vector entries 
in the term of the polynomial.
}
\seealso{ \code{\link{polynomp}}, \code{\link{polyback}}.}
\examples{
\dontrun{

polymat <-  polypar(m=3,p=2)

}
}
\keyword{ math }
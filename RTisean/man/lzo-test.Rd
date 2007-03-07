\name{lzo.test}
\alias{zeroth} %old routine name
\alias{lzo.test}
\title{ Modeling data trough a zeroth order ansatz }
\description{Makes a zeroth order ansatz and estimates the one step prediction errors
 of the model on a multivariate time series.}
\usage{
lzo.test(series, l, x = 0, m=c(1,2), c, d = 1, n, S = 1, k = 30, r, f = 1.2, s = 1, C)
}
\arguments{
  \item{series}{a matrix or a vector.}
  \item{l}{number of points to use. }
  \item{x}{number of lines to be ignored. }
  \item{m}{a vector containing the number of components of the time series and the embedding dimension.  }
  \item{c}{a vector containing the columns to be read.}
  \item{d}{delay for the embedding. }
  \item{n}{number of points for which the error should be calculated. }
  \item{S}{temporal distance between the reference points. }
  \item{k}{minimal numbers of neighbors for the fit. }
  \item{r}{neighborhood size to start with. }
  \item{f}{factor to increase the neighborhood size if not enough neighbors were found. }
  \item{s}{steps to be forecasted. }
  \item{C}{width of causality window. }
}
\details{
The function searches for all neighbors of the point to be forecasted and
takes as its image the average of the images of the neighbors. The given forecast errors are 
normalized to the standard deviations of each component. In addition to using a multicomponent
 time series, a temporal embedding is possible.  That's why the \code{m} argument needs two 
numbers as input, where the first one is the number of components and the second one the temporal embedding. 
}
\value{A matrix of \code{s} lines, containing the steps forecasted in the first column
and the normalized forecast errors in the following columns for each component of the vector.}
\seealso{ \code{\link{predict}}, \code{\link{xzero}}. }
\examples{
\dontrun{

dat <- henon(1000)
zerotherr <- lzo.test(dat, s = 20)
plot(zerotherr, t="l", xlab= "Steps", ylab= "Normalized error", main = "Zeroth order ansatz prediction errors")

}
}
\keyword{ ts }

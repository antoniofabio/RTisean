\name{surrogates}
\alias{surrogates}
\title{Making surrogate data}
\description{Reads data from a file and creates surrogate data with the same
 Fourier amplitudes and the same distribution of values.
 }
\usage{
surrogates(series, n = 1, i, S = FALSE, I, l, x = 0, m, c)
}

\arguments{
  \item{series}{a vector or a matrix.}
  \item{n}{number of surrogates.}
  \item{i}{number of iterations. }
  \item{S}{make spectrum exact rather than distribution. }
  \item{I}{seed for random numbers. }
  \item{l}{number of points. }
  \item{x}{number of values to be skipped. }
  \item{m}{number of columns to be read.}
  \item{c}{columns to be read. }
}
\details{
If \code{c} is chosen larger than 1, multivariate surrogates are prepared.
In that case, also the relative Fourier phases of the channels are matched to
those of the data. Since for finite length, distribution and spectral properties 
cannot be guaranteed exactly at the same time, the default output contains the 
iteration stage with the exact amplitudes. With the parameter \code{S} set, the stage with 
the exact spectrum (resp. relative phases) is given. Asymptotically, the difference
between both should converge to zero. 
It is advisable to select a suitable sub-sequence to minimize end effects by 
using \code{\link{endtoend}} before preparing surrogates}

\value{A vector or a matrix containing the surrogate data as columns.}

\references{
T. Schreiber and A. Schmitz, Improved surrogate data for nonlinearity 
tests, Phys. Rev. Lett. 77, 635 (1996). 
}
\seealso{ \code{\link{endtoend}}}
\examples{

\dontrun{
dat <- logistic(1000)
surr <- surrogates(dat)
Z <- cbind(dat,surr)[1:100,]
colnames(Z) <- c("data","surrogate")
plot.ts(Z,  mar.multi = c(0, 5.1, 1, 2.1 ),main="Logistic model time series")
}

}
\keyword{ math} 
\keyword{ ts } 

\name{xcor}
\alias{xcor}
\title{Cross correlations }
\description{Computes the cross correlations between two time series.}
\usage{
xcor(series, l, x = 0, c, D = 100)
}
\arguments{
\item{series}{a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read. }
  \item{D}{number of correlations.}
}
\value{A matrix containing the lags in the first column and the cross correlations
 devided by the standard deviations of both data sets in the second column.}
\examples{
\dontrun{

dat <- henon(1000)[,1]
noisydat <- dat + rnorm(1000)
plot(xcor(cbind(dat, noisydat),D=50),t="h", xlab="Lag", 
ylab="(Cross Corr.)/st.dev.", main="Cross correlation between
clean and noisy univariate Henon data")

}
}
\keyword{ math}
\keyword{ ts}
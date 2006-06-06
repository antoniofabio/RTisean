\name{RT_predict}
\alias{RT_predict}
\title{Simple nonlinear prediction}
\description{Performs locally constant predictions on scalar time series.}
\usage{
RT_predict(series, d, m, r, v, s = 1, l, x = 0, c = 1)}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{d}{delay.}
  \item{m}{embedding dimension. }
  \item{r}{absolute radius of neighborhoods. }
  \item{v}{same as fraction of standard deviation. }
  \item{s}{time steps ahead forecast. }
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read.}
}
\value{A vector containing the series of forecasted values.}
\seealso{  \code{\link{xzero}}, \code{\link{zeroth}}.}
\examples{
\dontrun{

dat <- lynx
pred <- RT_predict(dat, m = 3, d = 2, v = 1, s = 1)
plot(dat,main="Locally constant predictions",ylab="Lynx data",ylim=c(0,8000))
lines(1821:1934, pred, col=2)
legend(1820,7800, c("Raw data", "Filtered data"),fill=c(1,2),bty="n",cex=0.8)

}
}
\keyword{ ts }

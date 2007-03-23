\name{lfo.test}
\alias{onestep}
\alias{lfo.test}
\title{Local linear ansatz}
\description{Estimates the average one step prediction error of for a local linear ansatz fit.}
\usage{
lfo.test(series, l, x = 0, c=1, m = c(1,2), d = 1, n, k = 30, r, f = 1.2, s = 1, C)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read. }
  \item{m}{embedding dimension. } 
  \item{d}{delay for the embedding. }
  \item{n}{number of points for which the error should be calculated.}
  \item{k}{minimal numbers of neighbors for the fit. }
  \item{r}{neighborhood size to start with. }
  \item{f}{factor to increase the neighborhood size if not enough neighbors were found. }
  \item{s}{steps to be forecasted. }
  \item{C}{width of causality window.}
}
\value{A scalar corresponding to the relative forecast error, namely the forecast error 
devided by the standard deviation of the data.}
\examples{
\dontrun{

library(tseriesChaos)
dat <- rossler.ts
errors <- NULL
for(i in 1:100)
	errors[i] <- lfo.test(dat,s=i)

plot(errors,t="l",xlab="Forecasted steps", ylab="Relative error",main="Relative forecast error for a local linear fit")  

}
}
\keyword{ models}
\keyword{ ts}% 

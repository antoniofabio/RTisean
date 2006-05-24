\name{c2t}
\alias{c2t}
\title{ Maximum likelihood estimator from correlation sums}
\description{Reads the correlation integral, output of either  or \code{c1},
or \code{d2} and computes a maximum likelihood estimator (Takens' estimator). 
}
\usage{
c2t(lst)
}
\arguments{
  \item{lst}{the output of  \code{c1}, or \code{d2}.}
}
\value{
 A list of as many matrices as the number of embedding dimensions of \code{lst}.
 Each matrix contains the value of epsilon in the first column and the
 value of the Takens' estimator in the second column.
}
\seealso{  \code{\link{c2d}}, \code{\link{c2g}} }
\examples{
dat <- henon(l=10000,x=1000)
corsum <- d2(dat)
corsum <- corsum$.c2
\dontshow{try(c2t(corsum), TRUE)}
#FIXME: this fails on first call on win
takensest <- c2t(corsum)

plot(takensest$dim1,t="l",ylim=c(0,15.1), xlim=c(0.003,2.5), col=2,xlab="Epsilon",
ylab="Takens' Estimator",log="x", main="Maximum Likelihood Estimator Plot, embedding dims=(1,...,10)")
for (a in seq(1,10,1)) 
	points(takensest[[a]],t="l",col=2)
}
\keyword{ ts }

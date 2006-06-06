\name{project}
\alias{project}
\title{Projective nonlinear noise reduction}
\description{Performs locally projective nonlinear noise reduction of a time series.}
\usage{project(series, m, q, r, k, i = 1, l, x = 0, c = 1)}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{m}{embedding dimension.  }
  \item{q}{dimension of manifold.}
  \item{r}{radius of neighbourhoods. }
  \item{k}{minimal number of neighbours. }
  \item{i}{number of iterations. }
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read. }
}
\value{A matrix containing the filtered time series in the first column and the difference
between the original and the filtered time series in the second column.}

\references{ P. Grassberger, R. Hegger, H. Kantz, C. Schaffrath, and T. Schreiber, 
On noise reduction methods for chaotic data, 
Chaos 3, 127 (1993); 
Reprinted in: E. Ott, T. Sauer, and J. A. Yorke, eds., 
Coping With Chaos, Wiley, New York (1994) 
 }
\seealso{\code{\link{ghkss}} }
\examples{
\dontrun{

x <- 1:500
y <- cos(x/100)^2 - cos(x/200)+ rnorm(500)/10
filteredy <- project(y,m=7,q=2,k=10,r=1)
plot(x, y, t="l", xlab="Time", ylab="Time series", main="Projective nonlinear noise reduction")
lines(x, filteredy[,1], col=2,lwd=1.5)
legend(350,0, c("Noisy data","Filtered data"),fill=c(1,2), bty="n")

}
}
\keyword{ ts}
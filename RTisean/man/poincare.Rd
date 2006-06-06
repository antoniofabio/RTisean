\name{poincare}
\alias{poincare}
\title{Poincare section}
\description{
Makes a Poincare section for time continuous scalar data sets along one
of the coordinates of the embedding vector.}
\usage{
poincare(series, l, x = 0, c = 1, m = 2, d = 1, q, C = 0, a)
}
\arguments{
\item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{c}{column to be read. }
  \item{m}{embedding dimension. } 
  \item{d}{delay. }
  \item{q}{component for the crossing. }
  \item{C}{direction of the crossing (0: from below, 1: from above). }
  \item{a}{position of the crossing. }
}
\details{}
\value{
A matrix containing \code{m} columns for each cut. The first \code{m}-1
columns store the cohordinates of the vector at the crossing, the last one stores
the time between the last two crossings.}

\references{R. Hegger and H. Kantz, Embedding of sequences of time intervals, Europhys.
Lett. 38, 267 (1997). }
\examples{
\dontrun{

library(tseriesChaos)
dat <- rossler.ts
poincsect <- poincare(dat,a=1)
plot(poincsect,main="Poincare' section of Rossler data", xlab="Data cohordinates at the crossing", 
ylab="Time between the last two crossings")

}
}
\keyword{ math}
\keyword{ ts}
\keyword{ manip} 
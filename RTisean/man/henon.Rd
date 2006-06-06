\name{henon}
\alias{henon}
\title{Henon Model}
\description{Creates a time series from the Henon model.}
\usage{
henon(l = -1, A = 1.4, B = 0.3, X = -1, Y = -1, x = 10000)
}
\arguments{
  \item{l}{length of the time series. }
  \item{A}{parameter a of henon model. }
  \item{B}{parameter b of henon model. }
  \item{X}{initial x cohordinate. }
  \item{Y}{initial y cohordinate. }
   \item{x}{number of transients discarded. }
}

\value{ A matrix containing the two-dimensional time series.}

\examples{
\dontrun{

dat <- henon(l=10000,X=0.1,Y=0.2)
plot(dat, xlab="x(t)",ylab="y(t)", main="Henon Chaotic Attractor")

}
}
\keyword{ ts }
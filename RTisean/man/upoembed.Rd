\name{upoembed}
\alias{upoembed}
\title{Embedding periodic orbits using delay coordinates}
\description{Embeds periodic orbits, produced as output of \code{upo}, 
 using delay coordinates.}
\usage{
upoembed(data, d, m = 2, p = 1, l, x = 0, c = 1)
}
\arguments{
  \item{data}{the output containing the locations of periodic orbits as it
 is produced by  \code{upo}}
  \item{d}{delay. }
  \item{m}{embedding dimension.}
  \item{p}{period of orbit.}
  \item{l}{number of values to be read. }
  \item{x}{number of values to be skipped. }
  \item{c}{column to be read. }
}
\value{A matrix containing the embedded periodic orbits as columns.}
\note{This actually gives erroneous results}
\seealso{ \code{\link{upo}} }
\examples{
\dontrun{

dat <- henon(1000)
upoout <- upo(dat,m=2, v=0.1, n=70, p=6)
upoemb <- upoembed(upoout,d=1)

}
}
\keyword{ts}
\keyword{internal}
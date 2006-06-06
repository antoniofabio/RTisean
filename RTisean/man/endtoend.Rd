\name{endtoend}
\alias{endtoend}
\title{End-to-end mismatch of a time series}
\description{
Determines the effect of an end-to-end mismatch on the autocorrelation structure
for various sub-sequence lengths.}
\usage{
endtoend(series, l, x = 0, m, c = 1)
}
\arguments{
  \item{series}{a vector or a matrix. }
  \item{l}{number of data to use. }
  \item{x}{number of lines to be ignored. }
  \item{m}{number of columns to be read.}
  \item{c}{column to be read. }
}
\details{The sub-sequence length is decreased, only considering lengths 
which can be factorized with factors 2, 3 and 5. In the multivariate case, 
the mismatches is computed for each channel seperately and then averaged.}
\value{A list of as many named vectors as the number of sub-sequences considered,
each composed by:
\item{length}{The length of the sub-sequence.}
\item{offset}{The optimal offset.}
\item{lost}{ Percentage of removed points over the total number of points. }
\item{jump}{The contribution of the mismatch to the total power in the sub-sequence.}
\item{slip}{ The contribution of the mismatch in the first derivative.}
\item{weighted}{The weighted averaged mismatch.}
}
%\seealso{\code{\link{surrogates}}}
\examples{
\dontrun{

dat <- henon(1000)
mismatch <- endtoend(dat,m=2)

}
}
\keyword{ math } 
\keyword{ ts } 
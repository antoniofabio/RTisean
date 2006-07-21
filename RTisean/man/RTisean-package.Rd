\name{RTisean-package}
\alias{RTisean-package}
\alias{RTisean}
\title{The RTisean package}
\description{
The RTisean package: getting started
}
\details{
RTisean is an R interface to TISEAN executables (http://www.mpipks-dresden.mpg.de/~tisean/). 
TISEAN is a suite of C and Fortran routines for nonlinear time series analysis, coded and 
documented by Rainer Hegger, Holger Kantz and Thomas Schreiber. 
In RTisean, almost each TISEAN routine is wrapped in a conventional R
function (which silently calls TISEAN executables), with online help
and examples (thanks to Gianluca Gazzola).

To use this package, you must already have installed TISEAN executables, i.e.,
**download TISEAN binaries from the TISEAN web site, and unpack them to a local folder**
At the first call to a function in this package, you will be asked to specify the local directory where TISEAN binaries resides. Your answer will be stored in a settings file in your home directory, and used in all future functions calls.

Enjoy using RTisean!
}
\author{ TISEAN authors: Rainer Hegger, Holger Kantz, Thomas Schreiber. R interface code by Antonio, Fabio Di Narzo, R documentation and examples by Gianluca Gazzola }
\keyword{ ts }

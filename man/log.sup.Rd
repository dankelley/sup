% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sup.R
\name{log.sup}
\alias{log.sup}
\title{Logarithm function}
\usage{
\method{log}{sup}(x, base = exp(1))
}
\arguments{
\item{x}{An uncertain value, in one of three possible forms:
(1) an object of class \code{"sup"}, created with \code{\link[=as.sup]{as.sup()}},
(2) a numeric vector of length two containing a value as the first
entry and its uncertainty as the second entry, or
(3) a numeric vector of length 1, in which case the uncertainty
is assumed to be zero.}

\item{base}{Numerical value giving the base of he logarithm,
with the default yielding the natural logarithm.}
}
\value{
A two-element vector of class \code{"sup"}, which
contains result of the computation as the first entry,
and the uncertainty as the second.  (This class definition
means that printing the item will produce a a well-formated
display.)
}
\description{
Logarithm function
}
\examples{
log(as.sup(2, 0.2))
}

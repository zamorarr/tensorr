% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{col_apply}
\alias{col_apply}
\title{Apply a function to columns of a matrix}
\usage{
col_apply(x, f, ...)
}
\arguments{
\item{x}{matrix}

\item{f}{function to apply to each column}

\item{...}{extra args to f}
}
\description{
Convenience wrapper around apply. Always returns a matrix.
}
\keyword{internal}

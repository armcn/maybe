% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maybe.R
\name{maybe_map}
\alias{maybe_map}
\alias{fmap}
\title{Evaluate a function on a maybe value}
\usage{
maybe_map(.m, .f, ...)

fmap(.m, .f, ...)
}
\arguments{
\item{.m}{A maybe value}

\item{.f}{A function to apply to the maybe value}

\item{...}{Named arguments for the function \code{.f}}
}
\value{
A maybe value
}
\description{
Evaluate a function on a maybe value
}
\examples{
just(9) \%>\% maybe_map(sqrt)
nothing() \%>\% maybe_map(sqrt)
}

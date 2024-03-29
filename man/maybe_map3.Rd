% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maybe.R
\name{maybe_map3}
\alias{maybe_map3}
\title{Evaluate a ternary function on three maybe values}
\usage{
maybe_map3(.m1, .m2, .m3, .f, ...)
}
\arguments{
\item{.m1}{A maybe value}

\item{.m2}{A maybe value}

\item{.m3}{A maybe value}

\item{.f}{A ternary function to apply to the maybe values}

\item{...}{Named arguments for the function \code{.f}}
}
\value{
A maybe value
}
\description{
Evaluate a ternary function on three maybe values
}
\examples{
maybe_map3(just(1), just(2), just(3), function(x, y, z) x + y + z)
maybe_map3(nothing(), just(2), just(3), function(x, y, z) x / y * z)
}

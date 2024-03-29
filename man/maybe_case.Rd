% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maybe.R
\name{maybe_case}
\alias{maybe_case}
\title{Unwrap and call a function on a maybe value or return a default}
\usage{
maybe_case(.m, .f, default)
}
\arguments{
\item{.m}{A maybe value}

\item{.f}{A function to apply to the maybe value in the case of 'Just'}

\item{default}{A default value to return in the case of 'Nothing'}
}
\value{
The return value of the 'Just' function or the default value
}
\description{
Unwrap and call a function on a maybe value or return a default
}
\examples{
just(1:10) \%>\% maybe_case(mean, 0)
nothing() \%>\% maybe_case(mean, 0)
}

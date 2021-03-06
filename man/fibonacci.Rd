% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{fibonacci}
\alias{fibonacci}
\alias{fibonacci2}
\title{Fibonacci's sequence}
\usage{
fibonacci(n)

fibonacci2(n)
}
\arguments{
\item{n}{a positive integer}
}
\description{
Calculates the nth number of Fibonacci's sequence. \code{fibonacci2} is
the closed form (and vectorized).
}
\examples{
fibonacci(20)
sapply(1:20, fibonacci)
fibonacci2(1:20)

}

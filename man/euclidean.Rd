\name{euclidean}
\alias{euclidean}
\title{
Euclidean Algorithm for GCD Calculation
}
\description{
This function calculates the greatest common divisor (GCD) of two numeric values using Euclidean's algorithm.
}
\usage{
euclidean(a, b)
}
\arguments{
  \item{a}{
A numeric scalar representing the first number.
}
  \item{b}{
A numeric scalar representing the second number.
}
}
\details{
The Euclidean algorithm calculates the greatest common divisor of two given numbers by continuously replacing the larger number with its remainder when divided by the smaller number until the remainder equals zero. The last non-zero remainder is the greatest common divisor.
}
\value{
The function returns a single numeric value which is the greatest common divisor of \code{a} and \code{b}.
}
\references{
https://en.wikipedia.org/wiki/Euclidean_algorithm
}
\author{
Nisal Amashan Rajapaksha
}
\note{
This exercise is done as a part of coursework for 732A94.
}

\seealso{
https://search.r-project.org/CRAN/refmans/numbers/html/extGCD.html
}

\examples{
## Example of usage:
euclidean(56, 98)  # Returns 14
}

\keyword{math}
\keyword{GCD}
\keyword{algorithms}
\concept{Euclidean algorithm}
\concept{greatest common divisor}

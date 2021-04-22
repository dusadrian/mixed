\name{missing_values}
\alias{missing_values}
\alias{missing_range}
\alias{missing_values<-}
\alias{missing_range<-}

\title{Query / Define missing values}

\usage{
missing_values(x)

missing_values(x) <- value

missing_range(x)

missing_range(x) <- value
}
\arguments{
\item{x}{A vector.}

\item{value}{A vector of existing (real) values that should be considered as missing
(for \code{missing_values}) or a numeric vector of length two giving the  outer limits
of the range of values that are to be considered as missing (for \code{missing_range}).}

}
\value{
\code{missing_values()} returns all values that are defined as missing, through either
method (individually or using a range).
\code{missing_range()} returns only the outer limits of the numerical range for the 
values to be considered as missing.

}
\description{
Query / Define missing values
}
\details{
These functions are specific to this package and signal a fundamental difference from
the corresponding functions \code{na_values()} and \code{na_range()}, in package
\pkg{labelled}.

While in that package values are identified as missing using the base function
\code{is.na()}, the values are nonetheless existing and contribute to calculating,
for instance, measures of central tendency such as the mean or a median.

By contrast, in this package every value that is defined as missing is actually
stored as a missing value, using the tagged NAs introduced in package \pkg{haven}.

These values are also treated as existing when printed on the screen, or when checked
for value (in)equality.

To differentiate from packages \pkg{haven} and \pkg{labelled}, such an object is
marked with a class \code{mixed_labelled}.
}

\examples{

x <- labelled(c(1:5, -1), labels = c(Good = 1, Bad = 5, DK = -1))
x
missing_values(x) <- -1
missing_values(x)
x

is.na(x)

missing_values(x) <- NULL
x
missing_range(x) <- c(-Inf, -1)
missing_range(x)
x

missing_values(x)

}

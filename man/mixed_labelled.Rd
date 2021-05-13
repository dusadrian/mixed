\name{mixed_labelled}
\alias{mixed_labelled}
\alias{is_mixed}
\alias{as_mixed}
\alias{unmix}

\title{Mixed labelled vectors}
\description{Use mixed labelled vectors to analyse social science data}

\usage{
mixed_labelled(
    x = double(),
    labels = NULL,
    na_values = NULL,
    na_range = NULL,
    label = NULL,
    ...
)

is_mixed(x)

as_mixed(x, ...)

unmix(x)
}

\arguments{
\item{x}{A numeric vector to label, or a mixed labelled vector (for \code{unmix})}

\item{labels}{A named vector or \code{NULL}. The vector should be the same type
as \code{x}. Unlike factors, labels don't need to be exhaustive: only a fraction
of the values might be labelled.}

\item{na_values}{A vector of values that should also be considered as missing.}

\item{na_range}{A numeric vector of length two giving the (inclusive) extents
of the range. Use \code{-Inf} and \code{Inf} if you want the range to be
open ended.}

\item{label}{A short, human-readable description of the vector.}

\item{...}{Other arguments used by various other methods.}
}


\details{
The \code{mixed_labelled} class is very similar to the class \code{haven_labelled_spss}
from package \pkg{haven}. It has exactly the same arguments, but it features a fundamental
difference in the treatment of (defined) missing values.

Whereas in package \code{haven} the values are identified as missing by the base function
\code{is.na()}, they are in fact stored in the vector as actual (existing, non-missing)
values.

Despite being identified as missing, they do contribute to calculating various statistical
measures (for instance measures of central tendency) and consequently they also are
interpreted as real numbers by any statistical analysis (for instance in a regression).

By contrast, in the class \code{mixed_labelled} values are stored as tagged NA values,
and interpreted as actual existing values when testing for (non)equality.

The function \code{unmix()} has effect only for objects of class \code{mixed_labelled},
by removing this class and restoring the tagged NAs into their original numeric values.
It outputs an object as if it was created with \code{labelled_spss()}.

Since tagged NA values are stored as \code{double}, declaring missing values for character
variables will not work. In such situations, the only alternative is to use the function
\code{labelled_spss} from package \pkg{haven}. Character varibles with defined missing
values are specific to SPSS only, and are not supported by any other statistical software.
}

\examples{

x <- mixed_labelled(c(1:5, -1),
                    labels = c(Good = 1, Bad = 5, DK = -1),
                    na_values = -1)
x

is.na(x)

x > 0

x == -1


x <- labelled(c(1:5, -1), labels = c(Good = 1, Bad = 5, DK = -1))
na_values(x) <- -1
x


# a labelled_spss object can also be coerced to mixed
x <- labelled_spss(c(1:5, -1),
                    labels = c(Good = 1, Bad = 5, DK = -1),
                    na_values = -1)
x <- as_mixed(x)
x


# already defined new missing values are automatically tagged
x <- c(x, 2, -1)
x

}

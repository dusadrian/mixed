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

unmix(x, haven = FALSE)
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

\item{haven}{Logical, coerce the unmixed object to class "haven_labelled".}

\item{...}{Other arguments used by various other methods.}
}


\details{
The \code{mixed_labelled} objects are very similar to the \code{haven_labelled_spss} objects
from package \bold{haven}. It has exactly the same arguments, but it features a fundamental
difference in the treatment of (declared) missing values.

In package \bold{haven}, existing values are treated as if they were missing. By contrast,
in package \bold{mixed} the NA values are treated as existing values.

This difference is fundamental and points to an inconsistency in package \bold{haven}:
while existing values can be identified as missing using the function \code{is.na()}, they
are in fact present in the vector and other packages (most importantly the core ones) will
not know these values should be treated as missing.

Consequently, the existing values are interpreted as missing only by package haven.
Statistical procedures will use those values as just like they were valid values.

Package \bold{mixed} approaches the problem in exactly the opposite way: instead of
treating existing values as missing, it treats (certain) NA values as existing. This way,
it does not run into the risk of using values which should not be used.

The function \code{unmix()} replaces the NA entries into their original numeric values, and
drops all attributes related to missing values: \code{na_values}, \code{na_range} and
\code{na_index}.
}

\examples{

x <- mixed_labelled(c(1:5, -1),
                    labels = c(Good = 1, Bad = 5, DK = -1),
                    na_values = -1)
x

is.na(x)

x > 0

x == -1


# when newly added values are already declared as missing, they are automatically coerced
c(x, 2, -1)

# switch NAs with their original values
unmix(x)

}

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
    label = NULL
)

is_mixed(x)

as_mixed(x, ...)

unmix(x)
}

\arguments{
\item{x}{A numeric vector to label (for \code{mixed_labelled}), a labelled
vector (for \code{as_mixed}), or a mixed labelled vector (for \code{unmix})}

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

Whereas in the \code{haven} class the values are identified as missing by the base function
\code{is.na()}, they are actually present in the vector as real (existing, non-missing)
values.

Despite being identified as missing, they do contribute to calculating various statistical
measures (for instance measures of central tendency) and consequently they also are
interpreted as real numbers by any statistical analysis (for instance in a regression).

By contrast, in the class \code{mixed_labelled} values are stored as tagged NA values, as
introduced in package \pkg{haven}, but they are interpreted as real existing values when
testing for (non)equality.

To achieve that, an additional attribute called \code{tagged_values} is stored in the
variable, which is a named vector where the values are the original numbers and the names
are the one letter tags used by the \pkg{haven} function \code{tagged_na()}.

The function \code{unmix()} has effect only for objects of class \code{mixed_labelled},
by removing this class and restoring the tagged NAs into their original numeric values.

A labelled object is coerced to a \code{mixed_labelled} class only if it does not contain
tagged NA values. Missing values should be either real numbers declared as missing values,
or they can be tagged NAs from the very beginning but mixing both within the same variable
is not a very good idea.

Using the arguments \code{na_values} and \code{na_range} directly with the function
\code{labelled()} triggers the creation of a mixed labelled object.

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

attr(x, "tagged_values")

x <- labelled(c(1:5, -1), labels = c(Good = 1, Bad = 5, DK = -1))
missing_values(x) <- -1
x


# a labelled_spss object can also be coerced to mixed
x <- labelled_spss(c(1:5, -1),
                    labels = c(Good = 1, Bad = 5, DK = -1),
                    na_values = -1)
x <- as_mixed(x)
x


# it can be created directly with haven's labelled()
x <- labelled(c(1:5, -1),
                labels = c(Good = 1, Bad = 5, DK = -1),
                na_values = -1)
x


# new values that are already defined as missing are
# automatically tagged
x <- c(x, 2, -1)

is.na(x)

}

\name{mixed-package}

\alias{mixed-package}

\alias{mixed}

\docType{package}

\title{
Mixed labelled data
}

\description{
A package that extend the \code{haven_labelled} class introduced in package
\pkg{haven}, to deal with mixed labelled objects where existing values that
are defined as missing are treated as both real and missing values, at the
same time.

This package strives for as much compatibility as possible with packages
\pkg{haven} and \pkg{labelled}, implementing a different philosophy about
the nature and interpretation of defined missing values.

To mark this all important difference and know when this package takes
precedence, users will first notice the \code{mixed_labelled} class when
printed in the R console. Second, the tagged NA values are printed differently:
while in the original package these are displayed as \code{NA(a)}, in this
package they are displayed simply as a character with a preceding dot, for
instance \code{.a}

}

\details{
    \tabular{ll}{
    Package: \tab mixed\cr
    Type: \tab Package\cr
    Version: \tab 0.5\cr
    Date: \tab 2021-04-18\cr
    License: \tab GPL-v3\cr
    }
}
\author{
Adrian Dusa

Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
}

\keyword{misc}

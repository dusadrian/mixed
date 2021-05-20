\name{Tagged NAs}

\alias{tag}
\alias{has_tag}
\alias{get_tag}
\alias{untag}

\title{Set, check and extract tagged NAs}

\description{
Improved versions of the original functions \code{tagged_na()}, \code{is_tagged_na()} and
\code{na_tag()} from package \bold{\code{haven}}.
}

\usage{
tag(...)

has_tag(x, tag = NULL)

get_tag(x)

untag(x, numeric = FALSE)
}

\arguments{
  \item{...}{Vectors containing the tags for the missing value}
  \item{x}{A numeric vector}
  \item{tag}{If \code{NULL}, will only return true if the tag has this value.}
  \item{numeric}{Force output to numeric.}
}

\details{
Apart from the regular missing values in R (signaled with \code{NA}), package
\bold{\code{haven}} introduced the concept of tagged NAs, by storing one additional byte
of information in the NA value.

As useful as they are, the \bold{\code{haven}} functions have limitations. The first
and the most obvious, it accepts only one character as a tag. As it turns out, it is
actually possible to store two additional bytes instead of only one.

This makes it possible to store at least two, and in some situations up to four
characters as a tag. More importantly, numbers themselves can be used as "tags",
as long as their absolute value is less than 32768. Larger numbers are internally tagged
as characters, with a corresponding \code{large_numbers} attribute being added.

Theoretically, special characters could be used as tags because most would fit in two
bytes. In practice however, it is difficult to guess or to introduce information about
character encodings, and most of the times these characters lead to "invalid multibyte"
errors. To circumvent these issues, tagged characters are restricted to ASCII only.
}

\author{
Adrian Dusa
}

\value{
If all tags are numeric, the function \code{get_tag()} will return a numeric vector,
otherwise it returns a character vector.
}

\references{
Goldberg, David (1991) "What Every Computer Scientist Should Know About Floating-point Arithmetic",
ACM Computing Surveys vol.23, no.1, pp.5-48, \doi{10.1145/103162.103163}
}

\examples{
tag("ab")
# [1] NA(ab)

tag(-98)
# [1] NA(-98)

get_tag(tag("ab"))
# [1] "ab"

get_tag(tag(-98))
# [1] -98

x <- c(5:1, tag('a'), tag(-98), NA)
x
# [1]       5       4       3       2       1   NA(a) NA(-98)      NA

is.na(x)
# [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE

has_tag(x)
# [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE

has_tag(x, -98)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE

get_tag(x)
# [1] NA    NA    NA    NA    NA    "a"   "-98" NA

sort(x)
# [1] 1 2 3 4 5

sort(x, na.last = TRUE)
# [1]  1  2  3  4  5 NA

sort(x, na.last = TRUE, na_values.last = TRUE)
# [1]       1       2       3       4       5 NA(-98)   NA(a)      NA
}


\keyword{functions}

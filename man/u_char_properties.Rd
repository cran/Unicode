\name{u_char_properties}
\alias{u_char_info}
\alias{u_char_property}
\alias{u_char_properties}
\title{Unicode Character Properties}
\description{
  Get the properties of Unicode characters.
}
\usage{
u_char_info(x)
u_char_properties(x, which)
u_char_property(x, which)
}
\arguments{
  \item{x}{an \R object which can be coerced to a \code{\link{u_char}}
    vector of Unicode characters via \code{\link{as.u_char}}.}
  \item{which}{a character vector or string (for
    \code{u_char_property}), respectively, with the possibly abbreviated
    names of Unicode properties.
  }
}
\value{
  For \code{u_char_info}, a data frame with variables giving the Code
  (\code{Code}) and the \sQuote{basic} Unicode properties Name, General
  Category, Canonical Combining Class, Bidi Class, Decomposition,
  Numeric Value Decimal Digit, Numeric Value Digit, Numeric Value,
  Bidi Mirrored, Unicode 1 Name, ISO Comment, Simple Uppercase Mapping,
  Simple Lowercase Mapping, and Simple Titlecase Mapping, with names
  obtained by replacing white spaces by underscores (e.g.,
  \code{Bidi_Class}.)

  For \code{u_char_properties}, a data frame with the values of the
  specified properties.

  For \code{u_char_property}, the values of the specified property.
}
\note{
  Currently, only the property values of a subset of all Unicode
  character properties can be obtained.  Use
  \code{names(Unicode:::u_char_property_db)} to list the properties
  available in addition to the basic ones.
}
\references{
  Unicode Character Database (\url{http://www.unicode.org/ucd/})
}
\examples{
## When was the Euro sign added to Unicode?
x <- u_char_from_name("EURO SIGN")
u_char_property(x, "Age")
}

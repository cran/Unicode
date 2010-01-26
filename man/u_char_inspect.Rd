\name{u_char_inspect}
\alias{u_char_inspect}
\title{Unicode Character Inspection}
\description{
  Inspect Unicode characters.
}
\usage{
u_char_inspect(x)
}
\arguments{
  \item{x}{an \R object which can be coerced to a \code{\link{u_char}}
    vector of Unicode characters via \code{\link{as.u_char}}.}
}
\value{
  A data frame with variables \code{Code}, \code{Name} and \code{Char},
  giving the code and name of the given characters and the \R character
  vectors corresponding to the code points.
}
\examples{
## Who has ever seen a capital sharp s?
x <- u_char_from_name(c("LATIN SMALL LETTER SHARP S",
                        "LATIN CAPITAL LETTER SHARP S"))
u_char_inspect(x)
## (Does this display anything useful?)
}

\name{u_char_match}
\alias{u_char_match}
\alias{\%uin\%}
\title{Unicode Character Matching}
\description{
  Match Unicode characters to Unicode character ranges.
}
\usage{
u_char_match(x, table, nomatch = NA_integer_)
x \%uin\% table
}
\arguments{
  \item{x}{an \R object which can be coerced to a \code{\link{u_char}}
    vector of Unicode characters via \code{\link{as.u_char}}.}
  \item{table}{an \R object coercible to a \code{\link{u_char_range}}
    vector of Unicode character range via \code{\link{as.u_char_range}}.}
  \item{nomatch}{the value to be returned (after coercion to integer) in
    the case when no match is found.}
}
\details{
  \code{u_char_match} returns a vector of the positions of the (first)
  matches of the Unicode characters given by \code{x} (after coercion
  via \code{\link{as.u_char}}) to the Unicode character ranges given by
  \code{table} (after coercion via \code{\link{as.u_char_range}}).

  \code{\%uin\%} returns a logical vector indicating if there was a
  match or not.
}

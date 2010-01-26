\name{u_char_basics}
\alias{as.u_char}
\alias{as.u_char_range}
\alias{as.u_char_seq}
\alias{u_char}
\alias{u_char_range}
\alias{u_char_seq}
\title{Unicode Character Objects}
\description{
  Data structures and basic methods for Unicode character data.
}
\usage{
as.u_char(x)
as.u_char_range(x)
as.u_char_seq(x, sep = NA_character_)
}
\arguments{
  \item{x}{R objects coercible to the respective Unicode character data
    types, see \bold{Details}.}
  \item{sep}{a character string.}
}
\details{
  Package \pkg{Unicode} provides three basic classes for representing
  Unicode characters: \code{u_char} for vectors of Unicode characters,
  \code{u_char_range} for vectors of Unicode character ranges, and
  \code{u_char_seq} for vectors of Unicode character sequences.  Objects
  from these classes are created via the respective coercion functions.

  \code{as.u_char} knows to coerce integers or hex strings (with or
  without a leading \samp{0x} or the \samp{U+} typically used for
  Unicode characters) giving the corresponding code points.  It can also
  handle Unicode character ranges, flattening them out into the
  corresponding vector of Unicode characters.  To \dQuote{coerce} a
  UTF-8 encoded \R character string to the corresponding Unicode
  character object, use coercion on the result of obtaining the integer
  code points via \code{\link{utf8ToInt}}.

  \code{as.u_char_range} knows to coerce character strings of single
  Unicode characters or a Unicode range expression with the hex codes of
  two Unicode characters collapsed by \samp{..} (currently, hard-wired).
  It can also handle \code{u_char} objects, coercing them to ranges of
  single code points.

  \code{as.u_char_seq} knows to coerce character strings with the hex
  codes of Unicode characters collapsed by a non-empty \code{sep}.  The
  default corresponds to using \samp{,} if the strings use surrounding
  angles, and \samp{ } otherwise.  If \code{sep} is empty or has length
  zero, the character strings are used as is, re-encoded in UTF-8 if
  necessary, and mapped to the corresponding Unicode character sequences
  using \code{\link{utf8ToInt}}.  \code{as.u_char_seq} can also handle
  Unicode character ranges (giving the corresponding flattened out
  Unicode character sequences), or lists of objects coercible to Unicode
  characters via \code{as.u_char}.

  All classes currently have \code{as.character}, \code{as.data.frame},
  \code{format}, \code{print} and \code{[} subscript methods.  More
  methods will be added eventually.
}
\value{
  For \code{as.u_char}, a \code{u_char} object giving a vector of
  Unicode characters.

  For \code{as.u_char_range}, a \code{u_char_range} object giving a
  vector of Unicode character ranges.

  For \code{as.u_char_seq}, a \code{u_char_seq} object giving a
  vector of Unicode character sequences.
}
\references{
  Unicode Character Database (\url{http://www.unicode.org/ucd/}),\cr
  \url{http://en.wikipedia.org/wiki/Unicode}
}
\examples{
x <- as.u_char_range(c("00AA..00AC", "01CC"))
x
## Corresponding Unicode character sequence object:
as.u_char_seq(x)
## Corresponding Unicode character object with all code points:
as.u_char(x)
## Inspect all Unicode characters in the range:
u_char_inspect(x)

## Turning R character strings into the respective Unicode character
## sequences:
as.u_char_seq(c("Austria", "Trantor"), "")
## which can then be subscripted "as usual", e.g.:
x <- as.u_char_seq(c("Austria", "Trantor"), "")[[1L]][c(3L, 5L)]
x
## To reassemble the character strings:
intToUtf8(x)
}

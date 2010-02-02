\name{casefuns}
\encoding{UTF-8}
\alias{u_to_lower_case}
\alias{u_to_upper_case}
\alias{u_to_title_case}
\alias{u_case_fold}
\title{Unicode Case Conversions}
\description{
  Default Unicode algorithms for case conversion.
}
\usage{
u_to_lower_case(x)
u_to_upper_case(x)
u_to_title_case(x)
u_case_fold(x)
}
\arguments{
  \item{x}{\R objects (see \bold{Details}).}
}
\details{
  These functions are generic functions, with methods for the Unicode
  character classes (\code{\link{u_char}}, \code{\link{u_char_range}},
  and \code{\link{u_char_seq}}) which suitably apply the case mappings
  to the Unicode characters given by \code{x}, and a default method
  which treats \code{x} as a vector of \dQuote{Unicode strings}, and
  returns a vector of UTF-8 encoded character strings with the results
  of the case conversion of the elements of \code{x}.

  Currently, only the unconditional case maps are available for
  conversion to lower, upper or title case: other variants may be added
  eventually.

  Currently, conversion to title case is only available for
  \code{\link{u_char}} objects.  Other methods will be added
  eventually (once the Unicode text segmentation algorithm is
  implemented for detecting word boundaries).

  Currently, \code{u_case_fold} only performs \emph{full} case folding
  using the Unicode case mappings with status \dQuote{C} and \dQuote{F}:
  other variants will be added eventually.
}
\value{
  For the methods for the Unicode character classes, a
  \code{\link{u_char_seq}} vector of Unicode character sequences with 
  the conversions of the characters in \code{x}.

  For the default method, a UTF-8 encoded character string with the
  results of the case conversions of the elements of \code{x}.
}
\examples{
## Latin upper case letters A to Z:
x <- as.u_char(as.u_char_range("0041..005A"))
## In case we did not know the code points, we could use e.g.
x <- as.u_char(utf8ToInt(paste(LETTERS, collapse = "")))
sapply(x, intToUtf8)
## Unicode character method:
sapply(u_to_lower_case(x), intToUtf8)
## Default method:
u_to_lower_case(LETTERS)

u_case_fold("Hi Dave.")

## More interesting stuff: sharp s.
u_to_upper_case("heiß")
## Note that the default full upper case mapping of U+00DF (LATIN SMALL
## LETTER SHARP S) is *not* to U+1E9E (LATIN CAPITAL LETTER SHARP S).
u_case_fold("heiß")
}

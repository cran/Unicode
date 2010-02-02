\name{u_char_names}
\alias{u_char_name}
\alias{u_char_from_name}
\alias{u_char_label}
\title{Unicode Character Names}
\description{
  Find the names or labels of Unicode characters, or Unicode characters
  by their name.
}
\usage{
u_char_name(x)
u_char_from_name(x, type = c("exact", "grep"), ...)
u_char_label(x)
}
\arguments{
  \item{x}{an \R object which can be coerced to a \code{\link{u_char}}
    vector of Unicode characters via \code{\link{as.u_char}} for
    \code{u_char_name} and \code{u_char_label};
    a character vector otherwise.
  }
  \item{type}{one of \code{"exact"} or \code{"grep"}, or an abbreviation
    thereof.
  }
  \item{...}{arguments to be passed to \code{\link{grepl}} when using
    this for pattern matching.
  }
}
\details{
  The Unicode Standard provides a convention for labeling code points
  that do not have character names (control, reserved, noncharacter,
  private-use and surrogate code points).  These labels can be obtained
  by \code{u_char_label}.
  
  By default, exact matching is used for finding Unicode characters by
  name.  When \code{type = "grep"}, \code{\link{grepl}} is used for
  matching \code{x} against the Unicode character names; for now, Hangul
  syllable and CJK Unified Ideograph names are ignored in this case.
}
\value{
  For \code{u_char_name} and \code{u_char_label}, a character vector
  with the names or labels, respectively, of the corresponding Unicode
  characters.

  For \code{u_char_from_name}, a \code{\link{u_char}} object giving the
  Unicode characters with name exactly matching the given names.
}
\examples{
x <- as.u_char(utf8ToInt("Austria"))
u_char_name(x)

## Derived Hangul syllable character names are also supported for
## finding characters by exact matching:
x <- u_char_name("0xAC00")
x
u_char_from_name(x)

## Find all Unicode characters with name matching 'DIGIT ONE'.
x <- u_char_from_name("\\bDIGIT ONE\\b", "g")
## And show their names.
u_char_name(x)
}

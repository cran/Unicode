\name{u_blocks}
\alias{u_blocks}
\title{Unicode Blocks}
\description{
  Unicode blocks.
}
\usage{
u_blocks(x)
}
\arguments{
  \item{x}{a character vector with the names of Unicode blocks.}
}
\value{
  If \code{x} is missing, a list of the Unicode blocks given as
  \code{\link{u_char_range}} Unicode character ranges, with the (full)
  block names as names.

  If \code{x} is given, a (sub)list of the specific Unicode blocks.
}
\seealso{
  \code{\link{u_char_property}} to find the block (property) of Unicode
  characters.
} 
\references{
  Unicode Character Database (\url{https://www.unicode.org/ucd/})
}

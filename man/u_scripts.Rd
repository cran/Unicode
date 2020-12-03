\name{u_scripts}
\alias{u_scripts}
\title{Unicode Scripts}
\description{
  Unicode scripts.
}
\usage{
u_scripts(x)
}
\arguments{
  \item{x}{a character vector with the names of Unicode scripts.}
}
\value{
  If \code{x} is missing, a list of the Unicode scripts given as
  \code{\link{u_char_range}} Unicode character ranges, with the (full)
  block names as names.

  If \code{x} is given, a (sub)list of the specific Unicode scripts.
}
\seealso{
  \code{\link{u_char_property}} to find the script (property) of Unicode
  characters.
} 
\references{
  Unicode Character Database (\url{https://www.unicode.org/ucd/})
}
\examples{
scripts <- u_scripts()
names(scripts)
## Total number of code points assigned to the scripts:
sort(sapply(scripts, function(s) sum(n_of_u_chars(s))),
     decreasing = TRUE)
}

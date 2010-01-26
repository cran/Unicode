\name{n_of_u_chars}
\alias{n_of_u_chars}
\title{Unicode Character Counts}
\description{
  Compute the number of Unicode characters (code points) in sequences
  or ranges of Unicode characters.
}
\usage{
n_of_u_chars(x)
}
\arguments{
  \item{x}{a vector of Unicode characters, character ranges, or
    character sequences.
  }
}
\value{
  An integer vector with the numbers of Unicode characters specified by
  the elements of \code{x}.
}
\examples{
## How many code points are assigned to the Latin and Cyrillic scripts?
x <- u_scripts(c("Latn", "Cyrl"))
## Numbers in the respective ranges:
n <- lapply(x, n_of_u_chars)
n
## Total number:
sapply(n, sum)
}

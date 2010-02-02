\name{tokenizers}
\alias{Unicode_alphabetic_tokenizer}
\title{Unicode Alphabetic Tokenizer}
\description{
  A simple Unicode alphabetic tokenizer.
}
\usage{
Unicode_alphabetic_tokenizer(x)
}
\arguments{
  \item{x}{a character vector.}
}
\details{
  Tokenization first replaces the elements of \code{x} by their Unicode
  character sequences.  Then, the non-alphabetic characters (i.e., the
  ones which do not have the Alphabetic property) are replaced by
  blanks, and the corresponding strings are split according to the
  blanks.
}
\value{
  A character vector with the tokenized strings.
}

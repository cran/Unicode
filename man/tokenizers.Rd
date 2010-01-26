\name{tokenizers}
\alias{Unicode_alpha_tokenizer}
\title{Unicode Alpha Tokenizer}
\description{
  A simple Unicode alphabetic tokenizer.
}
\arguments{
  \item{x}{a character vector.}
}
\details{
  Tokenization first replaces the elements of \code{x} by their Unicode
  character sequences.  Then, the non-alphabetic characters (i.e., the
  ones with a Unicode general category other than `Letter') are replaced
  by blanks, and the corresponding strings are split according to the
  blanks.
}
\value{
  A character vector with the tokenized strings.
}

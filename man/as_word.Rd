% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_word.R
\name{as_word}
\alias{as_word}
\title{as_word}
\usage{
as_word(x = NULL, sentence = F, hyphenate = T)
}
\arguments{
\item{x}{a numeric. Number to convert to english}

\item{sentence}{a Bool. If true, the first letter is capitalised}

\item{hyphenate}{a Bool. If true, compound numbers are hyphenated}
}
\description{
Takes a numeral and converts to a word with optional sentence case. Based off the English package.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_word.R
\name{simpl_frac}
\alias{simpl_frac}
\title{simpl_frac}
\usage{
simpl_frac(
  p,
  pattern = "{frac} {pc}\%",
  sentence = F,
  percent = T,
  digits = 2,
  denom = 1:20
)
}
\arguments{
\item{p}{a proportion}

\item{sentence}{If true, the first letter is capitalised}

\item{percent}{If true, true percent is pasted to the final value}

\item{denom}{valid denominator values, a sequence}

\item{round}{the number of digits to round summary percent to}
}
\description{
Simplified fractions for presentation
}

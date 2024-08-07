% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supportFunctions.R
\name{calculateShares}
\alias{calculateShares}
\title{Calculate shares based on discrete choice model.}
\usage{
calculateShares(totPrice, lambda, pref = NULL)
}
\arguments{
\item{totPrice}{total price of an option in a branch of the decision tree}

\item{lambda}{exponent that determines the price sensitivity of the decision model}

\item{pref}{optional use of generic preference fators}
}
\value{
share of option in a branch of the decision tree
}
\description{
Function works for the use of generic preferences as well as for inconvenience costs.
If no preferences are provided the function sets them to one which is equivalent to
pure description by inconvenience costs.
}
\author{
Johanna Hoppe
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergePrices.R
\name{toolMergePrices}
\alias{toolMergePrices}
\title{Merge REMIND-derived fuel prices with non-fuel costs.}
\usage{
toolMergePrices(
  gdx,
  REMINDmapping,
  REMINDyears,
  intensity_data,
  nonfuel_costs,
  module = "edge_esm",
  FE_Pricetab = NULL
)
}
\arguments{
\item{gdx}{path to REMIND binary output, gdx format}

\item{REMINDmapping}{mapping of REMIND regions to ISO3 country codes}

\item{REMINDyears}{range of REMIND timesteps}

\item{intensity_data}{logit level intensity data}

\item{nonfuel_costs}{logit level non-fuel costs}

\item{module}{GDX input is based on old "complex" module or new "edge_esm" module}

\item{FE_Pricetab}{ship an external csv that includes FE prices. The prices from the gdx file will be overwritten for affected regions.}
}
\description{
Merge REMIND-derived fuel prices with non-fuel costs.
}

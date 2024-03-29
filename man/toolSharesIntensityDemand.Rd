% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sharesIntensitiesDemand.R
\name{toolSharesIntensityDemand}
\alias{toolSharesIntensityDemand}
\title{Derive shares, demand and intensities from the logit tree.}
\usage{
toolSharesIntensityDemand(
  logit_shares,
  MJ_km_base,
  REMINDyears,
  scenario,
  demand_input = NULL
)
}
\arguments{
\item{logit_shares}{logit tree level shares}

\item{MJ_km_base}{logit tree level intensities}

\item{REMINDyears}{range of REMIND time steps}

\item{scenario}{REMIND GDP scenario}

\item{demand_input}{full REMIND CES level ES demand, optional. If not given, logit level demand output is normalized.}
}
\description{
Derive shares, demand and intensities from the logit tree.
}

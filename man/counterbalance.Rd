% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counterbalance.R
\name{counterbalance}
\alias{counterbalance}
\title{Counterbalance experiment}
\usage{
counterbalance(design, method = "latinsquare")
}
\arguments{
\item{design}{Experiment design}

\item{method}{Method for counterbalancing, defaults to latinsquare}
}
\description{
This will counterbalance the entire experiment using a latin square approach
by default. Right now it's actually the only method, but in the future this
can be expanded further. Future functionality will also allow for different
types of stimuli to be counterbalanced separately (eg, critical items vs
filler items).
}

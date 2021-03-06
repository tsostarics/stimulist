% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_experiment.R
\name{fill_experiment}
\alias{fill_experiment}
\title{Fill out experiment}
\usage{
fill_experiment(design, use_as_is = F)
}
\arguments{
\item{design}{Experiment design}

\item{use_as_is}{Whether to use all the generated combinations as trials. Use
when you plan to present all of the trials to every participant.}
}
\description{
After setting all the components of your experiment, generates a full table
of all the stimuli and saves it to the complete_experiment entry of the
design. The output of this function can then be passed to save_lists() to
save separate counterbalanced stimulus lists to use.
}

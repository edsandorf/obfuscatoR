% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_payouts.R
\name{calc_pr_guess}
\alias{calc_pr_guess}
\title{Calculate the probability that the observer will try to guess the rule}
\usage{
calc_pr_guess(expected_payout_obs, payout_obs_no_guess, deterministic)
}
\arguments{
\item{expected_payout_obs}{Vector of expected payout to the observer from 
guessing}

\item{payout_obs_no_guess}{The payout to the observer from not guessing}

\item{deterministic}{A boolean equal to TRUE if we treat the decision to
guess as deterministic. Defaults to TRUE.}
}
\value{
A vector with the probabilities that an observer will guess
}
\description{
The function calculates the probability that an observer will try to guess 
which rule governs the decision maker's actions. The function is meant for 
internal use only. It can be printed alongside the payouts calculated using
\code{\link{print_payout}} if \code{print_all = TRUE}.
}

#' Calculate expected payout to the observer
#' 
#' The function calculates the expected payout to the observer. The payout to
#' the observer depends on the posterior probabilities, i.e. the probability
#' of a rule conditional on observing an action, and the monetary payout for 
#' guessing correctly. The function is meant for internal use only. To calculate
#' the payout to the observer, use the wrapper function 
#' \code{\link{calculate_payouts}}. 
#' 
#' @param pr_rk_aj A matrix of posterior probabilities
#' @param pay_obs The pay to the observer for guessing correctly.
#'  
#' @return A vector of expected pays for each possible guess

calc_payout_obs <- function(pr_rk_aj, pay_obs) {
  tmp <- Rfast::colMaxs(pr_rk_aj, value = TRUE) * pay_obs
  names(tmp) <- paste0("E[Pay|", seq_len(ncol(pr_rk_aj)), "]")
  return(tmp)
}

#' Calculate expected payout to the decision maker
#' 
#' The function calculates the expected payout to the decision maker. The payout
#' to the decision maker depends on whether or not the observer tries to guess
#' the rule, and the monetary payout for choosing an action that leaves the 
#' observer clueless enough about the rule to refrain from guessing. The 
#' function is meant for internal use only. To calculate the payout to the 
#' decision maker, use the wrapper function \code{\link{calculate_payouts}}. 
#' 
#' @param pr_guess A vector of probabilities that the observer will guess.
#' @param pay_dm The pay to the decision maker if the observer does not guess.
#' 
#' @return A vector of expected payouts for each possible guess made by the
#'  observer

calc_payout_dm <- function(pr_guess, pay_dm) {
  tmp <- as.vector((1 - pr_guess) * pay_dm)
  names(tmp) <- paste0("E[Pay|", seq_len(length(pr_guess)), "]")
  return(tmp)
}

#' Calculate the probability that the observer will try to guess the rule
#' 
#' The function calculates the probability that an observer will try to guess 
#' which rule governs the decision maker's actions. The function is meant for 
#' internal use only. It can be printed alongside the payouts calculated using
#' \code{\link{print_payout}} if \code{print_all = TRUE}.
#' 
#' @param expected_payout_obs Vector of expected payout to the observer from 
#' guessing
#' @param payout_obs_no_guess The payout to the observer from not guessing
#' @param deterministic A boolean equal to TRUE if we treat the decision to
#' guess as deterministic. Defaults to TRUE. 
#' 
#' @return A vector with the probabilities that an observer will guess

calc_pr_guess <- function(expected_payout_obs, payout_obs_no_guess,
                          deterministic){
  
  pay_diff <- expected_payout_obs - payout_obs_no_guess
  
  if (deterministic) {
    tmp <- as.numeric(pay_diff > 0) + as.numeric(pay_diff == 0) * 0.5
  } else {
    tmp <- 1 / (1 + exp(-(pay_diff)))
  }
  
  names(tmp) <- paste0("Pr[G|", seq_len(length(expected_payout_obs)), "]")
  return(tmp)
}

#' Calculate payouts
#' 
#' The function is a wrapper function for \code{\link{calc_payout_obs}} and
#' \code{\link{calc_payout_dm}}, and exported to be used by the user. 
#' It calculates the expected payout to both 
#' observers and decision makers for each possible action undertaken by the 
#' decision maker, and the observers choice of whether or not to try and guess
#' the rule. 
#' 
#' @param entropy A list containing the entropy
#' @param pay_obs A numeric with pay to the observer for guessing correctly
#' @param pay_no_guess A numeric with pay to the observer for not guessing
#' @param pay_dm A numeric with pay to the decision maker if the observer does 
#' not guess
#' @param deterministic If TRUE a deterministic procedure is used to determine 
#' whether the observer tries to guess. Default is FALSE and the probability is
#' calculated using a logit expression
#' 
#' @return A list or list of lists where each list contains the payout to the 
#' observer and decision maker. 
#' 
#' @export

calculate_payouts <- function (entropy, pay_obs, pay_dm, pay_no_guess,
                               deterministic = FALSE) {
  
  if (!is.matrix(entropy) && !is.list(entropy)) {
    stop("design must be a matrix or a list of matrices")
  }
  
  if (is.list(entropy)) {
    
    payout <- lapply(entropy, function(x) {
      posterior <- attr(x, "pr_rk_aj")
      design <- attr(x, "design")
      c_rule <- attr(design, "c_rule")
      tmp <- design[c_rule, ]
      tmp <- ifelse(tmp == 0 | tmp == 1, 1, 0)
      payout_obs <- calc_payout_obs(posterior, pay_obs)
      pr_guess <- calc_pr_guess(payout_obs, pay_no_guess, deterministic)
      payout_dm <- calc_payout_dm(pr_guess, pay_dm) * tmp
      return(list(payout_obs = payout_obs,
                  payout_dm = payout_dm,
                  pr_guess = pr_guess))
    })
    
  } else {
    posteriors <- attr(entropy, "pr_rk_aj")
    design <- attr(entropy, "design")
    c_rule <- attr(design, "c_rule")
    tmp <- design[c_rule, ]
    tmp <- ifelse(tmp == 0 | tmp == 1, 1, 0)
    payout_obs <- calc_payout_obs(posteriors, pay_obs)
    pr_guess <- calc_pr_guess(payout_obs, pay_no_guess, deterministic)
    payout_dm <- calc_payout_dm(pr_guess, pay_dm) * tmp
    payout <- list(list(payout_obs = payout_obs,
                        payout_dm = payout_dm,
                        pr_guess = pr_guess))
  }
  return(payout)
}

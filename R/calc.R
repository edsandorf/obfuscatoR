#' Calculate Pr(a_j|r_k)
#' 
#' The function calculates the probability of an action conditional on a given
#' rule. The function is for internal use only and cannot be called externally.
#' The resulting matrix will be supplied as an attribute to the entropy measure
#' see \code{\link{calc_entropy}}. 
#' 
#' @references 
#' Equation X in Chorus et al. (xxxx)
#'
#' @inheritParams calc_entropy
#' 
#' @return An r x a matrix of probabilities 

calc_pr_aj_rk <- function(ra_mat) {
    rows <- nrow(ra_mat)
    cols <- ncol(ra_mat)
    
    pr_aj_rk <- matrix(ra_mat >= 0) +
        (matrix(ra_mat == 0) * (1 / Rfast::rowsums(ra_mat >= 0)))
    
    rownames(pr_aj_rk) <- paste0("R", seq_len(rows))
    colnames(pr_aj_rk) <- paste0("A", seq_len(cols))
    return(pr_aj_rk)
}

#' Calculate Pr(r_k|a_j)
#' 
#' The function calculates the probability of a rule conditional on observing
#' a given action. The function is for internal use only and cananot be called
#' externally. The resulting matrix will be supplied as an attribute to the 
#' entropy measure see \code{\link{calc_entropy}}. 
#' 
#' @references 
#' Equation X in Chorus et al. (xxxx)
#'
#' @param pr_aj_rk A matrix with the probabilities of actions conditional on a
#' given rule. Calculated using \code{\link{calc_pr_aj_rk}}
#' @inheritParams calc_entropy
#' 
#' @return An r x a matrix of probabilities 

calc_pr_rk_aj <- function(pr_aj_rk, priors) {
    rows <- nrow(pr_aj_rk)
    cols <- ncol(pr_aj_rk)
    
    tmp <- pr_aj_rk * priors
    pr_rk_aj <- t(t(tmp) / Rfast::colsums(tmp))
    
    rownames(pr_rk_aj) <- paste0("R", seq_len(rows))
    colnames(pr_rk_aj) <- paste0("A", seq_len(cols))
    return(pr_rk_aj)
}

#' Calculate Shannon's Entropy
#' 
#' This function is used to calculate Shannon's Entropy. The objective of the 
#' decision maker is to maximize the entropy of the observer. 
#'
#' @param ra_mat A matrix with rows equal to the number of rules and columns
#'  equal to the number of actions
#' @param priors A vector of prior probabilities. Defaults to NULL.
#' 
#' @return Returns a vector of entropies for each possible action with the
#' following attributes:
#' \enumerate{
#'   \item ra_mat
#'   \item priors
#'   \item pr_aj_rk
#'   \item pr_rk_aj
#' }
#' 
#' 
#' @export

calc_entropy <- function(ra_mat, priors = NULL) {
    if (is.null(ra_mat)) stop("You must supply a matrix of rules and actions.")
    if (priors != NULL && length(priors) != nrow(ra_mat)) {
        stop("The length of priors is not equal to the number of rules.")
    }
    
    #   Define the dimensions of ra_mat
    rows <- nrow(ra_mat)
    cols <- ncol(ra_mat)
    
    #   Calculate the Pr(a_j|r_k)
    pr_aj_rk <- calc_pr_aj_rk(ra_mat)
    
    #   Check priors
    if (is.null(priors)) {
        priors <- rep((1 / rows), times = rows)
        names(priors) <- paste0("R", seq_len(rows))
    }
    
    #   Calculate Pr(r_k|a_j)
    pr_rk_aj <- calc_pr_rk_aj(pr_aj_rk, priors)
    
    #   Calculate Shannon's Entropy
    tmp <- pr_rk_aj * log(pr_rk_aj, base = 10)
    tmp[is.nan(tmp)] <- 0
    entropy <- -Rfast::colsums(tmp)
    names(entropy) <- paste0("A", seq_len(cols))
    
    #   Attache attributes
    attributes(entropy) <- list(ra_mat = ra_mat,
                                priors = priors,
                                pr_aj_rk = pr_aj_rk,
                                pr_rk_aj = pr_rk_aj)
    
    return(entropy)
}
# WRITE TEST FOR ENTROPY USING A KNOWN MATRIX!


#' Function for calculaton the pay to the observer
#' 
#' This function is used to calculate the expected pay to the observer
#' @param prob_rule_action A matrix containing the conditional probability of a rule given an observed action.
#'  This matrix is an output from . 
#' @param pay_observer The pay to the observer for guessing correctly. Usually passed through design_opt 
#' @return A vector of expected pays for each possible guess

calculate_pay_observer <- function(prob_rule_action, pay_observer){
    tmp <- Rfast::rowMaxs(prob_rule_action, value = TRUE) * pay_observer
    names(tmp) <- paste("E[Pay|", seq_len(ncol(prob_rule_action)), "]", sep = "")
    return(tmp)
}

#' Function for calculaton the pay to the decision maker
#' 
#' This function is used to calculate the expected pay to the decision maker
#' @param prob_guessing The probability that the observer will make a guess. This vector is passed from \code{\link{calculate_prob_guess}}.
#' @param pay_decision_maker The pay to the decision maker if the observer does not make a guess.
#' @return A vector of expected pays for each possible guess

calculate_pay_decision_maker <- function(prob_guessing, pay_decision_maker){
    tmp <- (1 - prob_guessing) * pay_decision_maker
    names(tmp) <- paste("E[Pay|", seq_len(length(prob_guessing)), "]", sep = "")
    return(tmp)
}

#' Function for calculating the probability of guessing
#' 
#' This function is used to calculate the probability that the observer will make a guess 
#' as to what rule governs a decision maker's action.
#' 
#' @param expected_pay_observer Vectro of expected pay calculated by \code{\link{calculate_pay_observer}}.
#' @param pay_observer_no_guess The payout to the observer if she refrains from guessing
#' @param design_opt A list of design options that govern the creation of the obfuscation games. 

calculate_prob_guess <- function(expected_pay_observer, pay_observer_no_guess, design_opt){
    pay_diff <- expected_pay_observer - pay_observer_no_guess
    if(design_opt$deterministic){
        tmp <- as.numeric(pay_diff > 0) + as.numeric(pay_diff == 0) * 0.5
    } else {
        tmp <- 1 / (1 + exp(-(pay_diff)))
    }
    
    names(tmp) <- paste("Pr[G|", seq_len(length(expected_pay_observer)), "]", sep = "")
    return(tmp)
}


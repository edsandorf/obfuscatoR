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
    
    tmp_01 <- matrix(as.numeric(ra_mat > 0), nrow = rows)
    tmp_02 <- matrix(as.numeric(ra_mat == 0), nrow = rows)
    
    pr_aj_rk <- tmp_01 + tmp_02 * (1 / Rfast::rowsums(ra_mat >= 0))
    
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

calc_entropy <- function(ra_mat, priors = NULL) {
    if (is.null(ra_mat)) stop("You must supply a matrix of rules and actions.")

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
    entropy <- structure(-Rfast::colsums(tmp),
                         names = paste0("A", seq_len(cols)),
                         ra_mat = ra_mat,
                         priors = priors,
                         pr_aj_rk = pr_aj_rk,
                         pr_rk_aj = pr_rk_aj)
    
    return(entropy)
}

#' Calculate the entropy
#' 
#' This function is a wrapper for calc_entropy.
#'
#' @param ra_mat A matrix with rows equal to the number of rules and columns
#'  equal to the number of actions or a list of such matrices.
#' @param priors A vector of prior values. If ra_mat is a list of matrices,
#' priors can be a matrix with rows equal to the length of ra_mat and columns
#' equal to the number of rules.
#'
#' @return A list of of vectors of entropies for each possible action with the
#' following attributes:
#' \enumerate{
#'   \item ra_mat
#'   \item priors
#'   \item pr_aj_rk
#'   \item pr_rk_aj
#' }
#'
#' @export

calculate_entropy <- function(ra_mat, priors = NULL) {
    if (is.null(ra_mat)) {
        stop("ra_mat is not supplied!")
    }
    
    if (!is.matrix(ra_mat) && !is.list(ra_mat)) {
        stop("ra_mat must be a matrix or a list of matrices")
    }
    
    if (is.matrix(ra_mat)) {
        ra_mat <- list(ra_mat)
    }
    
    all_matrices <- all(unlist(lapply(ra_mat, function(x) is.matrix(x))) == TRUE)
    if (!all_matrices) {
        stop("Not all elements of ra_mat are matrices.")
    }
    
    if (!is.null(priors)) {
        dims <- do.call(rbind, lapply(ra_mat, function(x) {
            dim(x)
        }))
        
        if (is.matrix(priors)) {
            dims_priors <- dim(priors)
            if (all(dims[, 1] != dims_priors[2]) && length(ra_mat) != dims_priors[1]) {
                warning("The matrix of priors has incorrect dimensions. Using uninformative priors. \n\n")
                priors <- NULL
            }
        } else {
            if (all(t(dims[, 1]) != length(priors))) {
                warning("The length of priors is not equal to the number of rules. Using uninformative priors.\n\n")
                priors <- NULL
            }
        }
        
    }
    
    lapply(seq_along(ra_mat), function(i) {
        if (is.matrix(priors)) {
            prior_probs = priors[i, ]
        } else {
            prior_probs = priors
        }
        calc_entropy(ra_mat[[i]], priors = prior_probs)
    })
}

#' Calculate expected payout to the observer
#' 
#' The function calculates the expected payout to the observer.
#' 
#' @param pr_rk_aj A matrix with the probabilities of a rule conditional on an
#' observed action
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
#' The function calculates the expected payout to the decision maker.
#' 
#' @param pr_guess A vector with the probability that the observer will make a 
#' guess. 
#' @param pay_dm The pay to the decision maker if the observer does 
#' not make a guess.
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
#' The function calculates the probability that an observer will try to make a
#' guess at which rule governs the decision maker's actions. 
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
        stop("ra_mat must be a matrix or a list of matrices")
    }
    
    if (is.list(entropy)) {

        payout <- lapply(entropy, function(x) {
            posterior <- attr(x, "pr_rk_aj")
            payout_obs <- calc_payout_obs(posterior, pay_obs)
            pr_guess <- calc_pr_guess(payout_obs, pay_no_guess, deterministic)
            payout_dm <- calc_payout_dm(pr_guess, pay_dm)
            return(list(payout_obs = payout_obs,
                        payout_dm = payout_dm,
                        pr_guess = pr_guess))
        })
        
    } else {
        posteriors <- attr(entropy, "pr_rk_aj")
        payout_obs <- calc_payout_obs(posteriors, pay_obs)
        pr_guess <- calc_pr_guess(payout_obs, pay_no_guess, deterministic)
        payout_dm <- calc_payout_dm(pr_guess, pay_dm)
        payout <- list(list(payout_obs = payout_obs,
                            payout_dm = payout_dm,
                            pr_guess = pr_guess))
    }
    return(payout)
}

#' Calculate Pr(a_j|r_k)
#' 
#' The function calculates the probability of an action conditional on a given
#' rule and is part of calculating the entropy of an action. The function is 
#' meant for internal use only.
#'
#' @inheritParams calculate_entropy
#' 
#' @return An r x a matrix of probabilities 

calc_pr_aj_rk <- function(design) {
    rows <- nrow(design)
    cols <- ncol(design)
    
    tmp_01 <- matrix(as.numeric(design > 0), nrow = rows)
    tmp_02 <- matrix(as.numeric(design == 0), nrow = rows)
    
    pr_aj_rk <- tmp_01 + tmp_02 * (1 / Rfast::rowsums(design >= 0))
    
    rownames(pr_aj_rk) <- paste0("R", seq_len(rows))
    colnames(pr_aj_rk) <- paste0("A", seq_len(cols))
    return(pr_aj_rk)
}

#' Calculate Pr(r_k|a_j)
#' 
#' The function calculates the probability of a rule conditional on observing
#' a given action and is part of calculating the entropy of an action. This 
#' probability is also referred to as the posterior probability. The function
#' is meant for internal use only. 
#'
#' @param pr_aj_rk A matrix with the probabilities of actions conditional on a
#' given rule. 
#' @inheritParams calculate_entropy
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
#' The function calculates Shannon's Entropy. The function is
#' meant for internal use only. To calculate the entropy for each action in the 
#' design, please use the wrapper function \code{\link{calculate_entropy}}
#'
#' @inheritParams calculate_entropy
#' 
#' @return Returns a vector of entropies for each possible action with the
#' following attributes:
#' \enumerate{
#'   \item design
#'   \item priors
#'   \item pr_aj_rk
#'   \item pr_rk_aj
#' }

calc_entropy <- function(design, priors = NULL) {
    if (is.null(design)) stop("You must supply a matrix of rules and actions.")

    #   Define the dimensions of design
    rows <- nrow(design)
    cols <- ncol(design)
    
    #   Calculate the Pr(a_j|r_k)
    pr_aj_rk <- calc_pr_aj_rk(design)
    
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
                         design = design,
                         priors = priors,
                         pr_aj_rk = pr_aj_rk,
                         pr_rk_aj = pr_rk_aj)
    
    return(entropy)
}

#' Calculate the entropy of each action in the design
#' 
#' The function is a wrapper for \code{\link{calc_entropy}} and is meant for
#' external use by the user. The goal for the decision maker is
#' to choose an action such that the observer is left as clueless as possible 
#' as to which rule governs his actions, i.e. maximize entropy.
#'
#' @param design A matrix with rows equal to the number of rules and columns
#'  equal to the number of actions or a list of such matrices.
#' @param priors A vector of prior values. If the design is a list of matrices,
#' priors can be a matrix with rows equal to the length of the design and columns
#' equal to the number of rules.
#'
#' @return A list of of vectors of entropies for each possible action with the
#' following attributes:
#' \enumerate{
#'   \item design
#'   \item priors
#'   \item pr_aj_rk
#'   \item pr_rk_aj
#' }
#'
#' @examples 
#'     design <- matrix(c(-1, -1, -1, -1,  1,
#'                        -1,  0,  0, -1,  0,
#'                        -1,  0, -1,  0,  0, 
#'                         0,  0, -1,  0, -1), nrow = 4L, byrow = TRUE)
#' 
#' calculate_entropy(design)
#' 
#' @export

calculate_entropy <- function(design, priors = NULL) {
    if (is.null(design)) {
        stop("design is not supplied!")
    }
    
    if (!is.matrix(design) && !is.list(design)) {
        stop("design must be a matrix or a list of matrices")
    }
    
    if (is.matrix(design)) {
        design <- list(design)
    }
    
    all_matrices <- all(unlist(lapply(design, function(x) is.matrix(x))) == TRUE)
    if (!all_matrices) {
        stop("Not all elements of design are matrices.")
    }
    
    if (!is.null(priors)) {
        dims <- do.call(rbind, lapply(design, function(x) {
            dim(x)
        }))
        
        if (is.matrix(priors)) {
            dims_priors <- dim(priors)
            if (all(dims[, 1] != dims_priors[2]) && length(design) != dims_priors[1]) {
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
    
    lapply(seq_along(design), function(i) {
        if (is.matrix(priors)) {
            prior_probs = priors[i, ]
        } else {
            prior_probs = priors
        }
        calc_entropy(design[[i]], priors = prior_probs)
    })
}

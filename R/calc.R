#' Function for calculating Shannon's Entropy
#'
#' This function is used to calculate Shannon's Entropy. The objective of the decision maker is to maximize the entropy of the observer.
#' @param rule_action_matrix A matrix with rows equal to the number of rules and columns equal to the number of actions
#' @param priors A vector of priors. Defaults to NULL and uninformative priors 1/R is used.
#' @export

calculate_entropy <- function(rule_action_matrix, priors = NULL){
    if(is.null(rule_action_matrix)) stop("The matrix with rules must be supplied!")
    if(length(priors) != nrow(rule_action_matrix)) stop("The length of the vector of priors is not equal to the number of rules!")

    rows <- nrow(rule_action_matrix)
    columns <- ncol(rule_action_matrix)

    names_rows <- str_c("R", seq_len(rows), sep = "")
    names_cols <- str_c("A", seq_len(columns), sep = "")
    rownames(rule_action_matrix) <- names_rows
    colnames(rule_action_matrix) <- names_cols


    #   Calculate Pr(a_j|r_k)
    prob_action_rule <- matrix(rule_action_matrix >= 0, nrow = rows) + (matrix(rule_action_matrix == 0, nrow = rows) * (1 / Rfast::rowsums(rule_action_matrix >= 0)))
    rownames(prob_action_rule) <- names_rows
    colnames(prob_action_rule) <- names_cols

    #   Check if Pr(r_k) is supplied, if not use uninformed priors
    if(is.null(priors)){
        priors <- rep((1 / rows), times = rows)
        names(priors) <- names_rows
    }

    #   Calculate Pr(r_k|a_j)
    tmp_matrix <- prob_action_rule * priors
    prob_rule_action <- t(t(tmp_matrix) / Rfast::colsums(tmp_matrix))
    rownames(prob_rule_action) <- names_rows
    colnames(prob_rule_action) <- names_cols

    #   Calculate Shannon's Entropy
    tmp_matrix <- prob_rule_action * log(prob_rule_action, base = 10)
    tmp_matrix[is.nan(tmp_matrix)] <- 0
    entropy <- -Rfast::colsums(tmp_matrix)
    names(entropy) <- names_cols

    #   Place in a list and return
    return(list(rule_action_matrix = rule_action_matrix,
                prob_action_rule = prob_action_rule,
                priors = priors,
                prob_rule_action = prob_rule_action,
                entropy = entropy))
}

#' Function for calculaton the pay to the observer
#' 
#' This function is used to calculate the expected pay to the observer
#' @param prob_rule_action A matrix containing the conditional probability of a rule given an observed action.
#'  This matrix is an output from \code{\link{calculate_entropy}}. 
#' @param pay_observer The pay to the observer for guessing correctly. Usually passed through design_opt 
#' @return A vector of expected pays for each possible guess

calculate_pay_observer <- function(prob_rule_action, pay_observer){
    tmp <- Rfast::rowMaxs(prob_rule_action, value = TRUE) * pay_observer
    names(tmp) <- str_c("E[Pay|", seq_len(ncol(prob_rule_action)), "]", sep = "")
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
    names(tmp) <- str_c("E[Pay|", seq_len(length(prob_guessing)), "]", sep = "")
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
    
    names(tmp) <- str_c("Pr[G|", seq_len(length(expected_pay_observer)), "]", sep = "")
    return(tmp)
}


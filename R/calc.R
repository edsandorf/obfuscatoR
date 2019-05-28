#' Function for calculating Shannon's Entropy
#'
#' This function is used to calculate Shannon's Entropy. The objective of the decision maker is to maximize the entropy of the observer.
#' @param rule_action_matrix A matrix with rows equal to the number of rules and columns equal to the number of actions
#' @param priors A vector of priors. Defaults to NULL and uninformative priors 1/R is used.

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

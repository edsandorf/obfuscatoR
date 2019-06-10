#' Function to create a rule-action matrix
#' 
#' The function takes as an input a list of design options and given a set of 
#' pre-programmed restrictions outputs one rule-action matrix. This matrix
#' is the design for one period of the obfuscation game. The package comes with
#' several other functions that are useful to evaluate the ra-matrix. The games 
#' will need to be manually inspected prior to implementation.
#' 
#' The function is for internal use only and not exported
#' 
#' @inheritParams check_design_opt
#' 
#' @examples 
#' design_opt_input <- list(rules = 5,
#'                          actions = 5,
#'                          considered_rule = 5,
#'                          min = 2, 
#'                          max = 3,
#'                          min_fit = 2,
#'                          obligatory = 2,
#'                          sd_entropy = 0.15)
#' 
#' construct_ra_mat(design_opt_input)
#' 
#' @return A rules-action matrix
#' 
#' @export

construct_ra_mat <- function(design_opt_input) {
    #   Check design_opt_input
    design_opt <- check_design_opt(design_opt_input)
    
    rules <- design_opt$rules
    actions <- design_opt$actions
    min_a <- design_opt$min
    max_a <- design_opt$max
    o_rules <- design_opt$obligatory
    c_rule <- design_opt$considered_rule
    
    #   Create the full factorial of allowed and prohibited actions
    action_list <- lapply(seq_len(actions), function(a){
        c(-1, 0)
    })
    action_mat <- as.matrix(expand.grid(action_list))
    
    #   Exclude actions outside the permitted range
    excluded_rows <- c(which(matrixStats::rowCounts(action_mat, value = 0) < min_a),
                       which(matrixStats::rowCounts(action_mat, value = 0) > max_a))
    action_mat <- action_mat[-c(excluded_rows), ]
    rows <- nrow(action_mat)
    
    max_rules <- rows + actions
    if (rows > max_rules) {
        stop("The maximum allowed number of rules is: ", max_rules)
    }
    
    #   Matrix of obligatory actions
    obligatory_mat <- matrix(-1, nrow = actions, ncol = actions)
    diag(obligatory_mat) <- 1

    #   Evaluate and update the matrix subject to a set of restrictions
    design_conditions <- rep(FALSE, 7)
    iter_counter <- 1
    while (any(design_conditions == FALSE)) {
        #   Randomly generate a design matrix
        ra_mat_tmp_1 <- action_mat[sample(rows,
                                          size = rules - o_rules,
                                          replace = FALSE), ]
        ra_mat_tmp_2 <- obligatory_mat[sample(actions,
                                          size = o_rules,
                                          replace = FALSE), ]
        ra_mat <- rbind(ra_mat_tmp_1, ra_mat_tmp_2)
        ra_mat <- ra_mat[sample(nrow(ra_mat)), ]
        
        #   Condition 1 - The considered rule cannot have an obligated action
        design_conditions[1L] <- ifelse(any(ra_mat[c_rule, ] == 1), FALSE, TRUE)
        
        #   Condition 2 - No action can be forbidden by every rule
        tmp <- abs(Rfast::colsums(ra_mat)) == rules
        design_conditions[2L] <- ifelse(any(tmp), FALSE, TRUE)
        
        #   Condition 3 - Allowable actions need to fit a min_a rules
        tmp <- which(ra_mat[c_rule, ] == 0)
        if (length(tmp) == 0) {
            design_conditions[3L] <- FALSE
        } else {
            #   rules - forbidden = allowed < min_allowed
            tmp <- (rules - Rfast::colsums(ra_mat[, tmp] == -1)) < min_a
            design_conditions[3L] <- ifelse(any(tmp), FALSE, TRUE)
        }
        
        #   Condition 4 - No duplicate actions allowed
        design_conditions[4L] <- ifelse(anyDuplicated(ra_mat, MARGIN = 2),
                                        FALSE, TRUE)
        
        #   Conditions 5 - 7 iff Condition 2 holds
        if (design_conditions[2L]) {
            #   Calculate the entropy
            entropy <- calc_entropy(ra_mat)
            i_max <- which(entropy == max(entropy))
            
            #   Cannot have more than one entropy max action
            if (length(i_max) == 1) {
                #   Condition 5 - max(entropy) must be permitted by c_rule
                design_conditions[5L] <- ifelse(ra_mat[c_rule, i_max] == -1,
                                                FALSE, TRUE)
                
                #   Condition 6 - the max entropy action has min pr_rk_aj
                max_post <- Rfast::colMaxs(attr(entropy, "pr_rk_aj"), value = TRUE)
                design_conditions[6L] <- ifelse(max_post[i_max] == min(max_post),
                                                TRUE, FALSE)
            }
            
            #   Condition 7 - ensure spread of the entropy measure
            design_conditions[7L] <- ifelse(stats::sd(entropy) < design_opt$sd_entropy,
                                            FALSE, TRUE)
            } else {
                design_conditions[5L] <- FALSE
                design_conditions[6L] <- FALSE
        }
        
        #   Check iterations and break if exceeds
        iter_counter <- iter_counter + 1
        if (iter_counter > design_opt$max_iter){
            cat("No suitable design found in the number of iterations specified")
            break
        }
    }
    
    #   Add row- and colnames prior to returning ra_mat
    ra_mat <- structure(ra_mat,
                        rownames = paste0("R", seq_len(rules)),
                        colnames = paste0("A", seq_len(actions)),
                        iter = iter_counter)
    return(ra_mat)
}

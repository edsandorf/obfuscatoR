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
#' @param design_opt A list of design options 
#' 
#' @return A rules-action matrix

construct_ra_mat <- function(design_opt) {
    rules <- design_opt$rules
    actions <- design_opt$actions
    min_a <- design_opt$min
    max_a <- design_opt$max
    o_rules <- design_opt$obligatory
    
    #   Create the full factorial of allowed and prohibited actions
    action_list <- lapply(seq_len(actions), function(a){
        c(1, 0)
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

    #   Evaluate and update the matrix
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
        
        
        #   Check iterations and break if exceeds
        iter_counter <- iter_counter + 1
        if (iter_counter > design_opt$max_iter){
            cat("No suitable design found in the number of iterations specified")
            break
        }
    }
    
    #   Add row- and colnames prior to returning ra_mat
    rownames(ra_mat) <- paste0("R", seq_len(rules))
    colnames(ra_mat) <- paste0("A", seq_len(actions))
    return(ra_mat)
}

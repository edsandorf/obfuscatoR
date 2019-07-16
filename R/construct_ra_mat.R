#' Function to create a rule-action matrix
#' 
#' The function creates a rule-action matrix (i.e. an obfuscation design)
#' subject to a list of pre-programmed restrictions. These restrictions are in
#' place to ensure that no invalid designs are created. Some of these restrictions
#' can be changed by the user by appropriately specifying the 
#' \code{design_opt_input}. Each matrix is a design for one period of the 
#' the obfuscation game. This function is for internal use only. To create
#' an obfuscation design, the user should use \code{\link{generate_designs}}.
#' 
#' @param  design_opt List of design options
#' 
#' @return A rules-action matrix

construct_design <- function(design_opt) {

    rules <- design_opt$rules
    actions <- design_opt$actions
    c_rule <- sample(seq_len(rules), 1L)
    
    #   Create the full factorial of allowed and prohibited actions
    action_list <- lapply(seq_len(actions), function(a){
        c(-1, 0)
    })
    action_mat <- as.matrix(expand.grid(action_list))
    
    # Check whether min or max number of rules have been specified
    if (!is.na(design_opt$min)) {
        excluded_rows <- which(matrixStats::rowCounts(action_mat, value = 0) < design_opt$min)
        action_mat <- action_mat[-c(excluded_rows), ]       
    }
    
    if (!is.na(design_opt$max)) {
        excluded_rows <- which(matrixStats::rowCounts(action_mat, value = 0) > design_opt$max)
        action_mat <- action_mat[-c(excluded_rows), ]
    }

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
        design <- action_mat[sample(rows, size = rules - design_opt$obligatory,
                                    replace = FALSE), ]
        
        #   Check if we are enforcing obligatory rules.
        if (design_opt$obligatory > 0) {
            design_oblig <- obligatory_mat[sample(actions,
                                                  size = design_opt$obligatory,
                                                  replace = FALSE), ]
            design <- rbind(design, design_oblig)
        }

        design <- design[sample(nrow(design)), ]
        
        #   Condition 1 - The considered rule cannot have an obligated action
        design_conditions[1L] <- ifelse(any(design[c_rule, ] == 1), FALSE, TRUE)
        
        #   Condition 2 - No action can be forbidden by every rule
        tmp <- abs(Rfast::colsums(design)) == rules
        design_conditions[2L] <- ifelse(any(tmp), FALSE, TRUE)
        
        #   Condition 3 - Allowable actions need to fit a min_fit rules
        tmp <- which(design[c_rule, ] == 0)
        if (length(tmp) == 0) {
            design_conditions[3L] <- FALSE
        } else {
            #   rules - forbidden = allowed < min_allowed
            tmp <- (rules - Rfast::colsums(design[, tmp, drop = FALSE] == -1)) < design_opt$min_fit
            design_conditions[3L] <- ifelse(any(tmp), FALSE, TRUE)
        }
        
        #   Condition 4 - No duplicate actions allowed
        design_conditions[4L] <- ifelse(anyDuplicated(design, MARGIN = 2),
                                        FALSE, TRUE)
        
        #   Conditions 5 - 7 iff Condition 2 holds
        if (design_conditions[2L]) {
            #   Calculate the entropy
            entropy <- calc_entropy(design)
            i_max <- which(entropy == max(entropy))
            
            #   Cannot have more than one entropy max action
            if (length(i_max) == 1) {
                #   Condition 5 - max(entropy) must be permitted by c_rule
                design_conditions[5L] <- ifelse(design[c_rule, i_max] == -1,
                                                FALSE, TRUE)
                
                #   Condition 6 - the max entropy action has min pr_rk_aj
                max_post <- Rfast::colMaxs(attr(entropy, "pr_rk_aj"), value = TRUE)
                design_conditions[6L] <- ifelse(max_post[i_max] == min(max_post),
                                                TRUE, FALSE)
            }
            
            #   Condition 7 - ensure spread of the entropy measure
            if (!is.na(design_opt$sd_entropy)) {
                design_conditions[7L] <- ifelse(stats::sd(entropy) < design_opt$sd_entropy,
                                                FALSE, TRUE)
            } else {
                design_conditions[7L] <- TRUE
            }

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
    
    #   Add row- and colnames prior to returning design
    design <- structure(design,
                        dimnames = list(paste0("R", seq_len(rules)),
                                        paste0("A", seq_len(actions))),
                        iter = iter_counter,
                        design_conditions = design_conditions,
                        c_rule = c_rule)
    return(design)
}

#' Check and set design_options
#' 
#' This function is run internally to set default design options when none are
#' specified by the user. The user should not run this function.
#' 
#' \describe{
#'   \item{rules}{Number of rules (i.e. rows)}
#'   \item{actions}{Number of actions (i.e. columns)}
#'   \item{min}{Minimum number of actions available for the considered rule}
#'   \item{max}{Maximum number of actions available for the considered rule}
#'   \item{min_fit}{Minimum number of rules fitting each permitted action
#'   conditional on the rule}
#'   \item{obligatory}{Number of rules with obligatory actions}
#'   \item{sd_entropy}{Specifies the standard deviation of the entropy values}
#'   \item{designs}{Number of designs to generate}
#'   \item{max_iter}{Maximum number of iterations before stopping search for
#'   designs}
#'   \item{seed}{A seed for the random number generator. Useful for
#'   replicability}
#' }
#' 
#' @inheritParams generate_designs
#' 
#' @return 
#' Returns a list of design options with the missing from input replaced by
#' default values
#' 

check_design_opt <- function(design_opt_input) {
    design_opt <- list(rules = NULL,
                       actions = NULL,
                       min = NA, 
                       max = NA,
                       min_fit = 0,
                       obligatory = 0,
                       sd_entropy = NA,
                       designs = 1,
                       max_iter = 1e5,
                       seed = NA)
    
    #   Fill in the user input
    design_opt[names(design_opt_input)] <- design_opt_input
    
    return(design_opt)
}

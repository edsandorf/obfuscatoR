#' Check and set design_options
#' 
#' This function is run internally to set default design options when none are
#' specified by the user.
#' 
#' \describe{
#'   \item{rules}{Number of rules (i.e. rows)}
#'   \item{actions}{Number of actions (i.e. columns)}
#'   \item{considered_rule}{The considered rule, i.e. specifies the row}
#'   \item{min}{Minimum number of actions available for the considered rule}
#'   \item{max}{Maximum number of actions available for the considered rule}
#'   \item{min_fit}{Minimum number of rules fitting each permitted action
#'   conditional on the rule}
#'   \item{obligatory}{Number of rules with obligatory actions}
#'   \item{sd_entropy}{Specifies the standard deviation of the entropy values}
#'   \item{pay_obs}{The pay to the observer for guessing}
#'   \item{pay_obs_no_guess}{The pay to the observer for not guessing}
#'   \item{deterministic}{Is the observer guessing deterministic}
#'   \item{pay_dm}{The pay to the decision maker if the observer does not guess}
#'   \item{designs}{Number of designs to generate}
#'   \item{max_iter}{Maximum number of iterations before stopping search for
#'   designs}
#'   \item{print_all}{Print all information about the design}
#'   \item{seed}{A seed for the random number generator. Useful for
#'   replicability}
#' }
#' 
#' @param design_opt_input A list of user supplied design options
#' 
#' @return 
#' Returns a list of design options with the missing from input replaced by
#' default values
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
#' check_design_opt(design_opt_input)
#' 
#' @export

check_design_opt <- function(design_opt_input) {
    design_opt <- list(rules = 5,
                       actions = 5,
                       considered_rule = 5,
                       min = 2, 
                       max = 3,
                       min_fit = 2,
                       obligatory = 2,
                       sd_entropy = 0.15,
                       pay_obs = 10,
                       pay_obs_no_guess = 5,
                       deterministic = FALSE,
                       pay_dm = 5,
                       designs = 1,
                       max_iter = 1e5,
                       print_all = TRUE,
                       seed = 3539)
    
    #   Fill in the user input
    design_opt[names(design_opt_input)] <- design_opt_input
    
    return(design_opt)
}
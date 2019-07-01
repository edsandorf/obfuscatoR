#' Prints the rules-action matrix
#' 
#' Prints the rule and action matrix. Depending on options, additional text is 
#' provided with information on the considered rule and/or the design generation 
#' process.
#' 
#' @param ra_mat A matrix with rows equal to the number of rules and columns
#' equal to the number of actions
#' @param c_rule The considered rule.
#' @param print_all If TRUE prints information on the number of iterations and 
#' and whether all design conditions were met. Default is FALSE
#' 
#' @examples 
#' design_opt_input <- list(rules = 4, 
#'                          actions = 5,
#'                          considered_rule = 3)
#'
#' design <- generate_designs(design_opt_input)
#' 
#' print_ra(design)
#' print_ra(design, 3)
#' print_ra(design, 3, TRUE)
#' 
#' @export

print_ra <- function(ra_mat, c_rule = NULL, print_all = FALSE) {
  cat(crayon::blue(crayon::bold("The rules-action matrix \n\n")))
  cat(crayon::blue("Rows: Rules \n"))
  cat(crayon::blue("Columns: Actions \n\n"))
  # Extract information before setting all attributes to NULL
  rows <- nrow(ra_mat)
  cols <- ncol(ra_mat)
  if (print_all) {
    iter <- attr(ra_mat, "iter")
    design_conditions <- attr(ra_mat, "design_conditions")
  }
  attributes(ra_mat) <- NULL
  ra_mat <- matrix(ra_mat, nrow = rows, ncol = cols,
                   dimnames = list(paste0("R", seq_len(rows)),
                                   paste0("A", seq_len(cols))))
  
  print(ra_mat)
  cat("\n")
  cat(crayon::green(paste0("The considered rule is ",
                           ifelse(is.null(c_rule), "N/A",
                                  crayon::bold(c_rule)), ".\n\n")))

  if (print_all) {
    cat(crayon::green(paste0("The design was found in ",
                             crayon::bold(iter),
                             " iterations. \n\n")))
    cat(crayon::green(paste0("All the design conditions were met: ",
                         crayon::bold(all(design_conditions)))))
  }  
}

#' Prints the entropy of the different actions 
#'
#' \code{print_entropy()} prints the entropy of the different actions and if 
#' desirable, will print all intermediary calculatations.
#' 
#' @param entropy The entropy measure from \code{calculate_entropy}
#' @param digits The number of digits to round to. Default 3. 
#' @param c_rule The considered rule. This defaults to NULL.
#' @param print_all If TRUE will print all information on intermediary 
#' calculations
#' 
#' @examples
#' ra_mat <- matrix(c(-1, -1, -1, -1,  1,
#'                    -1,  0,  0, -1,  0,
#'                    -1,  0, -1,  0,  0,
#'                     0,  0, -1,  0, -1), nrow = 4, byrow = TRUE)
#'
#' entropy <- calc_entropy(ra_mat)
#'
#' print_entropy(entropy)
#' print_entropy(entropy, digits = 4)
#' print_entropy(entropy, print_all = TRUE)
#'
#' @export

print_entropy <- function(entropy, digits = 3, c_rule = NULL, print_all = FALSE) {
  cols <- length(entropy)

  # Need to get all the attributes before stripping them if print all
  if (print_all) {
    ra_mat <- attr(entropy, "ra_mat")
    priors <- attr(entropy, "priors")
    pr_aj_rk <- attr(entropy, "pr_aj_rk")
    pr_rk_aj <- attr(entropy, "pr_rk_aj")
    rows <- nrow(ra_mat)
  }
  
  # Clear all attributes prior to printing
  attributes(entropy) <- NULL
  cat(crayon::blue(crayon::bold("Shannon's entropy \n\n")))
  names(entropy) <-  paste0("A", seq_len(cols))
  print(entropy, digits = digits)
  cat("\n")
  
  if (print_all) {
    print_ra(ra_mat, c_rule = c_rule, print_all = FALSE)
    
    cat(crayon::blue(crayon::bold("The vector of prior probabilities \n\n")))
    names(priors) <- paste0("R", seq_len(rows))
    print(priors, digits = digits)
    cat("\n")
    
    cat(crayon::blue(crayon::bold("The probability of an action conditional on a rule \n\n")))
    print(pr_aj_rk, digits = digits)
    cat("\n")
    
    cat(crayon::blue(crayon::bold("The probability of a rule conditional on observing an action, i.e. the posterior \n\n")))
    print(pr_rk_aj, digits = digits)
  }
  
}

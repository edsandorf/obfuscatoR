#' Prints the design
#' 
#' Takes a design or list of designs and prints them to the console. To store a 
#' design, please see \code{\link{save_design}}. Depending on the print options,
#' additional text is provided with information on the considered rule and/or
#' the design generation process.
#' 
#' @param design A matrix with rows equal to the number of rules and columns
#' equal to the number of actions
#' @param print_all If TRUE prints information on the number of iterations and 
#' and whether all design conditions were met. Default is FALSE
#' 
#' @examples 
#' design_opt_input <- list(rules = 4, 
#'                          actions = 5)
#'
#' design <- generate_designs(design_opt_input)
#' 
#' print_design(design)
#' print_design(design, TRUE)
#' 
#' @export

print_design <- function(design, print_all = FALSE) {
  cat(crayon::blue(crayon::bold("The rules-action matrix \n\n")))
  cat(crayon::blue("Rows: Rules \n"))
  cat(crayon::blue("Columns: Actions \n\n"))
  
  for (i in seq_along(design)) {
    design_tmp <- design[[i]]
    
    # Extract information before setting all attributes to NULL
    rows <- nrow(design_tmp)
    cols <- ncol(design_tmp)
    c_rule <- attr(design_tmp, "c_rule")
    
    if (print_all) {
      iter <- attr(design_tmp, "iter")
      design_conditions <- attr(design_tmp, "design_conditions")
    }
    attributes(design_tmp) <- NULL
    design_tmp <- matrix(design_tmp, nrow = rows, ncol = cols,
                     dimnames = list(paste0("R", seq_len(rows)),
                                     paste0("A", seq_len(cols))))
    
    print(design_tmp)
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
      cat("\n\n")
    } 
  }
}

#' Prints the entropy of the different actions 
#'
#' The function prints the vector of entropies for each possible action. 
#' Depending on printing options, additional information about the probability
#' calculations can be provided. 
#' 
#' @param entropy The entropy measure from \code{calculate_entropy}
#' @param digits The number of digits to round to. Default 3. 
#' @param print_all If TRUE will print all information on intermediary 
#' calculations
#' 
#' @examples
#' design <- matrix(c(-1, -1, -1, -1,  1,
#'                    -1,  0,  0, -1,  0,
#'                    -1,  0, -1,  0,  0,
#'                     0,  0, -1,  0, -1), nrow = 4, byrow = TRUE)
#'
#' entropy <- calculate_entropy(design)
#' 
#' print_entropy(entropy)
#' print_entropy(entropy, digits = 4)
#' print_entropy(entropy, print_all = TRUE)
#'
#' @export

print_entropy <- function(entropy, digits = 3, print_all = FALSE) {
  for (i in seq_along(entropy)) {
    entropy_tmp <- entropy[[i]]
    
    cols <- length(entropy_tmp)
    
    # Need to get all the attributes before stripping them if print all
    if (print_all) {
      design <- attr(entropy_tmp, "design")
      rows <- nrow(design)
      design <- list(design)
      priors <- attr(entropy_tmp, "priors")
      pr_aj_rk <- attr(entropy_tmp, "pr_aj_rk")
      pr_rk_aj <- attr(entropy_tmp, "pr_rk_aj")
    }
    
    # Clear all attributes prior to printing
    attributes(entropy_tmp) <- NULL
    cat(crayon::blue(crayon::bold("Shannon's entropy -- Design ", i, "\n\n")))
    names(entropy_tmp) <-  paste0("A", seq_len(cols))
    print(entropy_tmp, digits = digits)
    cat("\n\n")
    
    if (print_all) {
      print_design(design, print_all = FALSE)
      
      cat(crayon::blue(crayon::bold("The vector of prior probabilities \n\n")))
      names(priors) <- paste0("R", seq_len(rows))
      print(priors, digits = digits)
      cat("\n")
      
      cat(crayon::blue(crayon::bold("The probability of an action conditional on a rule \n\n")))
      print(pr_aj_rk, digits = digits)
      cat("\n")
      
      cat(crayon::blue(crayon::bold("The probability of a rule conditional on observing an action, i.e. the posterior \n\n")))
      print(pr_rk_aj, digits = digits)
      cat("\n\n")
    }
  }
}
 
#' Print the payouts
#'
#' The function formats and prints the payout to the observer and decision
#' maker. 
#' 
#' @param payout A list of calculated payouts
#' @param digits The number of digits to round to. Default 3. 
#' @param print_all If TRUE will print the probabilities of guessing
#'
#' @export

print_payout <- function (payout, digits = 3, print_all = FALSE) {
  for (i in seq_along(payout)) {
    payout_tmp <- payout[[i]]
    
    cat(crayon::blue(crayon::bold("Payout to the observer -- Design ", i, "\n\n")))
    print(payout_tmp[[1]], digits = digits)
    cat("\n\n")
    
    cat(crayon::blue(crayon::bold("Payout to the decision maker -- Design ", i, "\n\n")))
    print(payout_tmp[[2]], digits = digits)
    cat("\n\n")
    
    if (print_all) {
      cat(crayon::blue(crayon::bold("Probabilities of guessing -- Design ", i, "\n\n")))
      print(payout_tmp[[3]], digits = digits)
      cat("\n\n")
    }
  }
}

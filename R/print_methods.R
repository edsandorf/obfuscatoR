#' Prints the ra_matrix 
#' 
#' Prints the ra_matrix with the considered rule highlighted
#' 
#' @param ra_mat A matrix with rows equal to the number of rules and columns
#' equal to the number of actions
#' @param c_rule Defaults to the rule specified in design_opt.
#' @param all_info If TRUE prints informatino on the number of iterations and 
#' and whether all design conditions have been met. Default is FALSE
#' 
#' @output A nicely printed and formated matrix
#' 
#' @export

print_ra_mat <- function(ra_mat, c_rule = NULL, all_info = FALSE) {
  cat(crayon::blue(crayon::bold("The rules-action matrix \n\n")))
  cat(crayon::blue("Rows: Rules \n"))
  cat(crayon::blue("Columns: Actions \n\n"))
  # Extract information before setting all attributes to NULL
  rows <- nrow(ra_mat)
  cols <- ncol(ra_mat)
  if (all_info) {
    iter <- attr(ra_mat, "iter")
    design_conditions <- attr(ra_mat, "design_conditions")
  }
  attributes(ra_mat) <- NULL
  ra_mat <- matrix(ra_mat, nrow = rows, ncol = cols)
  rownames(ra_mat) <- paste0("R", seq_len(rows))
  colnames(ra_mat) <- paste0("A", seq_len(cols))
  
  print(ra_mat)
  cat("\n")
  cat(crayon::green(paste0("The considered rule is ",
                           ifelse(is.null(c_rule), "N/A",
                                  crayon::bold(c_rule)), ".\n\n")))

  if (all_info) {
    cat(crayon::green(paste0("The design was found in ",
                             crayon::bold(iter),
                             " iterations. \n\n")))
    cat(crayon::green(paste0("All the design conditions were met: ",
                         crayon::bold(all(design_conditions)))))
  }  
}

#' Prints the ra_matrix 
#' 
#' Prints the ra_matrix with the considered rule highlighted
#' 
#' @param ra_mat A matrix with rows equal to the number of rules and columns
#' equal to the number of actions
#' @param c_rule Defaults to the rule specified in design_opt. NOTE: If you are only 
#' evaluating a rules and action matrix, you must remember to set the c_rule or
#' it will automatically default to 5. 
#' 
#' @output A nicely printed and formated matrix
#' 
#' @export

print_ra_mat <- function(ra_mat, c_rule = NULL) {
  
  
}

#' Function for printing vectors and matrices 
#' 
#' @param X Input matrix
#' @param row_wise If TRUE populate something row_wise. Default FALSE
#' @param digits Number of digits for rounding
#' 
#' @export

print_object <- function(X, row_wise = FALSE, digits = 2){
  if(is.matrix(X)){
    
  }
}


#' Prints the ra_matrix 
#' 
#' Prints the ra_matrix with the considered rule highlighted
#' 
#' @ra_mat A matrix with rows equal to the number of rules and columns
#' equal to the number of actions
#' @c_rule Defaults to the rule specified in design_opt. NOTE: If you are only 
#' evaluating a rules and action matrix, you must remember to set the c_rule or
#' it will automatically default to 5. 
#' 
#' @output A nicely printed and formated matrix
#' 
#' @export

print_ra_mat <- function(ra_mat, c_rule = design_opt$considered_rule) {
  
  
}

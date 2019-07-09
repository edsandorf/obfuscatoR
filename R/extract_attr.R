#' Extract attributes
#' 
#' Extracts the attributes of objects nested in a list
#'
#' @param x A list of objects with attributes or an object with an attribute
#' @param str_attr A non-empty character string specifying which attribute is to
#' be extracted
#' 
#' @return Returns a list the length of x containing the specified attribute. If
#' the attribute does not exist, returns NULL
#'
#' @examples 
#' design_opt_input <- list(rules = 4, actions = 5)
#' design <- generate_designs(design_opt_input)
#' extract_attr(design, "design_conditions")
#' 
#' design_opt_input <- list(rules = 4, actions = 5, designs = 2)
#' design <- generate_designs(design_opt_input)
#' extract_attr(design, "design_conditions")
#' 
#' @export

extract_attr <- function (x, str_attr) {
  if (is.list(x)) {
    lapply(x, function (x) {
      attr(x, str_attr)
    })
  } else {
    attr(x, str_attr)
  }
}

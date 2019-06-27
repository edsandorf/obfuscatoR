#' Get the last element of a vector
#' 
#' \code{last} extracts the last element of a vector
#'
#' @param x A vector
#'
#' @examples 
#' x <- 1:4
#' last(x)
#' 
#' x <- c("hello", "my", "name", "is", "buttons")
#' last(x)
#' 
#' @export

last <- function (x) {
  x[length(x)]
}

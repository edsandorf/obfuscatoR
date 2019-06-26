#' Save obfuscation designs
#'
#' Function for storing designs. 
#'
#' @param x A design or list of designs
#' @param x_name A character string with the name and extension of the file to 
#' to be stored. If file extension is not provided the default is .csv
#' @param path A string giving the path to where the designs are stored. The 
#' default is the current working directory
#'
#'
#' 
#' @export

save_design <- function(x, x_name,  path = getwd()) {
  if (!is.character(x_name)) {
    stop(cat(crayon::red("x_name must be a character vector")))
  }
  
  if (!is.character(path)) {
    cat(crayon::yellow(crayon::bold("WARNING: "),
                       "Path is not a string. Defaults to current working dir."))
    path <- getwd()
  }
  
  # Check if extension is part of x_name
  
  # Get the file extension to call the correct function
  
}

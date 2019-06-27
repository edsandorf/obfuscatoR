#' Save obfuscation designs
#'
#' Writes the designs to separate .csv files. 
#'
#' @param x A design or list of designs
#' @param x_name A character string with the name of the file
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
  file_path <- unlist(stringr::str_split(x, "\\."))
  file_extension <- last(file_path)
  if (file_extension == "csv") {
    file_path <- file_path[-c(length(file_path))]
  }
  
  # Check for list of designs
  if (is.list(x)) {
    lapply(seq_along(x), function (i) {
      path <- stringr::str_c(file_path, "-design-", i, ".csv")
      readr::write_csv(x[[i]], path = path)
    })
  } else {
    path <- stringr::str_c(file_path, ".csv")
    readr::write_csv(x, path = path)
  }
}

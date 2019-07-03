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
  file_name <- unlist(stringr::str_split(x_name, "\\."))
  file_extension <- last(file_name)
  if (file_extension == "csv") {
    file_name <- file_name[-c(length(file_name))]
  }
  
  # Check for list of designs
  if (is.list(x)) {
    lapply(seq_along(x), function (i) {
      file_name <- stringr::str_c(file_name, "-design-", i, ".csv")
      path <- file.path(path, file_name)
      design_tmp <- tibble::as_tibble(x[[i]], rownames = NA)
      readr::write_csv(design_tmp, path = path)
    })
  } else {
    file_name <- stringr::str_c(file_name, ".csv")
    path <- file.path(path, file_name)
    design_tmp <- tibble::as_tibble(x, rownames = NA)
    readr::write_csv(design_tmp, path = path)
  }
}

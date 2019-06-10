#' Print package startup message
#' 
#' The function is called when the package is loaded through library or require.
#' 
#' @param libname Library name
#' @param pkgname Package name
#' 
#' @return Nothing

.onAttach <- function(libname, pkgname){
    installed_version <- utils::packageDescription("obfuscatoR", fields = "Version")
    
    # description <- readLines("https://raw.githubusercontent.com/edsandorf/obfuscatoR/master/DESCRIPTION")
    # remote_version <- gsub("Version:\\s*", "", description[grep('Version:', description)])
    remote_version <- "NA"
    
    packageStartupMessage("Welcome to obfuscatoR! \n\n",
                          "You are currently using version: ",
                          installed_version, "\n\n",
                          "The latest version is: ", remote_version, "\n\n",
                          "To access the latest version, please run \n",
                          "devtools::install_github('edsandorf/obfuscatoR') \n\n",
                          "To cite this package:")
}

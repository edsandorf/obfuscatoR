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
    
    description <- tryCatch({
        readLines("https://raw.githubusercontent.com/edsandorf/obfuscatoR/master/DESCRIPTION")
    }, warning = function(w) {
        return("NA")
    }, error = function(e) {
        return("NA")
    })
    
    if (length(description) == 1) {
        remote_version <- description
    } else {
        remote_version <- gsub("Version:\\s*", "", description[grep('Version:', description)])
    }
    
    packageStartupMessage("Welcome to obfuscatoR: An R Package for the Obfuscation Game \n\n",
                          "You are currently using version: ",
                          installed_version, "\n\n",
                          "The latest version is: ", remote_version, "\n\n",
                          "To access the latest version, please run \n",
                          "devtools::install_github('edsandorf/obfuscatoR') \n\n",
                          "To cite this package: Sandorf, E. D., Chorus, C. & van Cranenburgh, S., 2019, obfuscatoR: An R Package for the Obfuscation Game, https://CRAN.R-project.org/package=obfuscatoR \n")
}

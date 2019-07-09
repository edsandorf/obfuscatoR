#' Generate obfuscation designs
#' 
#' This is the main function to generate the obfuscation designs. It takes a 
#' list of design options as its only argument. See 
#' \code{\link{check_design_opt}} for default values. 
#' 
#' At the very minimum, the number of rules, actions and the considered rule
#' has to be supplied
#' 
#' @param design_opt_input A list of user supplied design options. Must at a 
#' minimum specify the number of rules, actions and the considered rule.
#' 
#' @return 
#' A list of matrices with rules and actions
#' 
#' @examples
#' design_opt_input <- list(rules = 4,
#'                          actions = 5)
#' 
#' generate_designs(design_opt_input)
#' 
#' @export

generate_designs <- function(design_opt_input = list()) {
    #   Check the design options
    design_opt <- check_design_opt(design_opt_input)
    
    #   Create a list of valid rule action matrices
    str_tmp <- stringr::str_split(Sys.time(), " ")[[1L]][2L]
    str_tmp <- unlist(stringr::str_split(str_tmp, ":"))
    number_tmp <- as.integer(stringr::str_c(str_tmp, collapse = ""))
    
    lst_ra_mat <- lapply(seq_len(design_opt$designs), function(i) {
        #   Set seed to ensure different designs or set for replicability
        if (is.na(design_opt$seed)) {
            set.seed(floor(number_tmp * runif(1)))
        } else {
            set.seed(design_opt$seed + i)
        }
        
        construct_ra_mat(design_opt)
    })
    
    return(lst_ra_mat)
}

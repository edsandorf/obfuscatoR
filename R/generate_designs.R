#' Generate obfuscation designs
#' 
#' This is the main function to generate the obfuscation designs. It takes a 
#' list of design options as its only argument. See 
#' \code{\link{check_design_opt}} for default values. 
#' 
#' @param design_opt_input A list of user supplied design options. The default
#' value is an empty list. 
#' 
#' @return 
#' A list of matrices with rules and actions
#' 
#' @examples
#' #   Example 1 - Using only default values
#' generate_designs()
#' 
#' #   Example 2 - Using some user supplied values
#' design_opt_input <- list(rules = 5,
#'                          actions = 5,
#'                          considered_rule = 5,
#'                          min = 2, 
#'                          max = 3,
#'                          min_fit = 2,
#'                          obligatory = 2,
#'                          sd_entropy = 0.15)
#' 
#' 
#' generate_designs(design_opt_input)
#' 
#' @export

generate_designs <- function(design_opt_input = list()) {
    #   Check the design options
    design_opt <- check_design_opt(design_opt_input)
    
    #   Set the seed for the RNG
    set.seed(design_opt$seed)
    
    #   Create a list of valid rule action matrices
    str_tmp <- stringr::str_split(Sys.time(), " ")[[1L]][2L]
    str_tmp <- unlist(stringr::str_split(str_tmp, ":"))
    number_tmp <- as.integer(stringr::str_c(str_tmp, collapse = ""))
    
    lst_ra_mat <- lapply(seq_len(design_opt$designs), function(i) {
        #   Set seed to ensure different designs
        set.seed(floor(number_tmp * stats::runif(1)))
        construct_ra_mat(design_opt)
    })
    
    return(lst_ra_mat)
}

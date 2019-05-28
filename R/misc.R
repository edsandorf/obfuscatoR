#' Function for printing vectors and matrices 
#' 
#' @param X A vector or matrix to be printed
#' @row_wise A boolean indicating whether to transpose the matrix before printing. Defaults to FALSE.
#' @digits Output is rounded to numer of digits. Default is 2. 

print_object <- function(X, row_wise = FALSE, digits = 2){
    if(is.matrix(X)){
        names_rows <- rownames(X)
        names_cols <- colnames(X)
        columns <- ncol(X)
    } else {
        names_cols <- names(X)
        columns <- length(X)
    }
    
    #   Set up and format the matrix
    tmp_matrix <- matrix(as.numeric(format(round(X, digits), nsmall = digits)),
                         ncol = columns)
    colnames(tmp_matrix) <- names_cols
    
    #   Check and print
    if(is.matrix(X)) rownames(tmp_matrix) <- names_rows
    if(row_wise){
        print(t(tmp_matrix))
    } else {
        print(tmp_matrix)
    }
}

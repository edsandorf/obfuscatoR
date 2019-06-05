testthat::test_that("calc_entropy() returns correct vector with attr", {
    #   Validated input matrix and goal vector
    ra_mat <- matrix(c(-1, -1, -1, -1,  1,
                       -1,  0,  0, -1,  0,
                       -1,  0, -1,  0,  0, 
                        0,  0, -1,  0, -1), nrow = 4L, byrow = TRUE)
    entropy_goal <- c(0.00000000, 0.47712125, 0.00000000, 0.30103000, 0.41269725)
    
    #   Calculate entropy and set attributes to NULL for testing purposes ONLY    
    entropy <- calc_entropy(ra_mat)
    attributes(entropy) <- NULL
    
    testthat::expect_equal(entropy, entropy_goal)
})
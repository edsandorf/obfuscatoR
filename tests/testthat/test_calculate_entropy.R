testthat::test_that("calculate_entropy() ", {
    #   Validated input matrix
    ra_mat <- matrix(c(-1, -1, -1, -1,  1,
                       -1,  0,  0, -1,  0,
                       -1,  0, -1,  0,  0, 
                        0,  0, -1,  0, -1), nrow = 4L, byrow = TRUE)
    
    entropy_goal_1 <- c(0.00000000, 0.47712125, 0.00000000, 0.30103000, 0.41269725)
    entropy <- calculate_entropy(ra_mat, priors = NULL)
    entropy_tmp_1 <- entropy[[1]]
    attributes(entropy_tmp_1) <- NULL
    testthat::expect_equal(entropy_tmp_1, entropy_goal_1)

    
    
    entropy_goal_2 <- c(0.000000000, 0.453122636, 0.000000000, 0.265294996, 0.415055189)
    entropy <- calculate_entropy(ra_mat, priors = c(0.2, 0.3, 0.15, 0.35))
    entropy_tmp_1 <- entropy[[1]]
    attributes(entropy_tmp_1) <- NULL
    testthat::expect_equal(entropy_tmp_1, entropy_goal_2)
    
    ra_mat <- list(ra_mat, matrix(c(-1, -1, -1, -1,  1,
                                    -1,  0,  0, -1, -1,
                                    -1, -1,  0,  0,  0,
                                    0,  0, -1, -1, -1), nrow = 4L, byrow = TRUE))
   
    entropy_goal_3 <- c(0.000000000, 0.301029996, 0.292285253, 0.000000000, 0.244219050)
    entropy <- calculate_entropy(ra_mat, priors = NULL)
    entropy_tmp_1 <- entropy[[1]]
    attributes(entropy_tmp_1) <- NULL
    testthat::expect_equal(entropy_tmp_1, entropy_goal_1)
    entropy_tmp_2 <- entropy[[2]]
    attributes(entropy_tmp_2) <- NULL
    testthat::expect_equal(entropy_tmp_2, entropy_goal_3)
    
    entropy_goal_4 <- c(0.000000000, 0.296583222, 0.290685441, 0.000000000, 0.257516354)
    priors <- matrix(c(0.20, 0.30, 0.15, 0.35,
                     0.30, 0.15, 0.35, 0.20), nrow = 2, byrow = TRUE)
    entropy <- calculate_entropy(ra_mat, priors = priors)
    entropy_tmp_1 <- entropy[[1]]
    attributes(entropy_tmp_1) <- NULL
    testthat::expect_equal(entropy_tmp_1, entropy_goal_2)
    entropy_tmp_2 <- entropy[[2]]
    attributes(entropy_tmp_2) <- NULL
    testthat::expect_equal(entropy_tmp_2, entropy_goal_4)
    
})

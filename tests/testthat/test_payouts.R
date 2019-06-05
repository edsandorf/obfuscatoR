testthat::test_that("Payouts are calculated correctly", {
    #   Validated input matrix
    ra_mat <- matrix(c(-1, -1, -1, -1,  1,
                       -1,  0,  0, -1,  0,
                       -1,  0, -1,  0,  0, 
                       0,  0, -1,  0, -1), nrow = 4L, byrow = TRUE)
    
    #   Define the goal vectors
    payout_obs_goal <- c(10, 3.3333333333, 10, 5, 6)
    pr_guess_F_goal <- c(0.99330715, 0.15886910, 0.99330715, 0.5, 0.73105858)
    pr_guess_T_goal <- c(1, 0, 1, 0.5, 1)
    payout_dm_F_goal <- c(0.03346425, 4.20565448, 0.03346425, 2.5, 1.34470711)
    payout_dm_T_goal <- c(0, 5, 0, 2.5, 0)
    
    #   Calculate the entropy and retrieve Pr(r_k|a_j)
    entropy <- calc_entropy(ra_mat)
    pr_rk_aj <- attr(entropy, "pr_rk_aj")
    
    #   Calculate the expected payout to the observer
    pay_obs <- 10
    payout_obs <- calc_payout_obs(pr_rk_aj, pay_obs)
    attributes(payout_obs) <- NULL
    
    #   Calculate the probabilities of guessing
    pay_obs_no_guess <- pay_dm <- 5
    
    pr_guess_F <- calc_pr_guess(payout_obs, pay_obs_no_guess, FALSE)
    attributes(pr_guess_F) <- NULL
    
    pr_guess_T <- calc_pr_guess(payout_obs, pay_obs_no_guess, TRUE)
    attributes(pr_guess_T) <- NULL
    
    #   Calculate the payout to the decision maker
    payout_dm_F <- calc_payout_dm(pr_guess_F, pay_dm)
    attributes(payout_dm_F) <- NULL
    payout_dm_T <- calc_payout_dm(pr_guess_T, pay_dm)
    attributes(payout_dm_T) <- NULL
    
    expect_equal(payout_obs, payout_obs_goal)
    expect_equal(pr_guess_F, pr_guess_F_goal)
    expect_equal(pr_guess_T, pr_guess_T_goal)
    expect_equal(payout_dm_F, payout_dm_F_goal)
    expect_equal(payout_dm_T, payout_dm_T_goal)
})
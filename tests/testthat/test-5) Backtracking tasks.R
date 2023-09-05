test_that(
  "mode_backtrack", {
    expect_equal(mode_backtrack, 1L)
  }
)

test_that(
  "max_after_backtrack", {
    expect_equal(length(max_after_backtrack), nrow(backtracks_df))
    expect_equal(max_after_backtrack[83], 304L)
  }
)

test_that(
  "even_odd_sd_len", {
    expect_equal(sum(even_odd_backtrack), nrow(backtracks_df))
    expect_equal(even_odd_backtrack[2] / even_odd_backtrack[1], 1.08699,
                 tol = 1e-3)
  }
)

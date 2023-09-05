test_that(
  "collatz_df is a data frame with correct dimensions", {
    expect_s3_class(collatz_df, "data.frame")
    expect_gte(nrow(collatz_df), 10000)
    expect_gte(ncol(collatz_df), 2)
  }
)

test_that(
  "Correct Collatz sequence generated", {
    x <- sample(1:10000, 5)
    y <- map_dbl(x, collatz::collatz_function)
    z <- map_dbl(x, \(x) gen_collatz(x)[2])
    expect_equal(y, z)
  }
)


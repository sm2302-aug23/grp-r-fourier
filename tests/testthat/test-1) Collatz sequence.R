test_that(
  "Throws error for invalid starting integers", {
    expect_error(gen_collatz(0))
    expect_error(gen_collatz(-83))
    expect_error(gen_collatz(3.14))
  }
)

test_that(
  "collatz_df contains start and seq", {
    expect_true(all(c("start", "seq") %in% names(collatz_df)))
  }
)

test_that(
  "collatz_df seq column is a list", {
    expect_type(collatz_df$seq, "list")
  }
)

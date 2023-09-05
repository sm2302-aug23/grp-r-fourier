test_that(
  "Validity of backtracks_df", {
    expect_equal(sum(nrow(backtracks_df) * top10longest), 662541477L)
  }
)

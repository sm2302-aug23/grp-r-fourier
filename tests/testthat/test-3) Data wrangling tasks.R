test_that(
  "top10longest",{
    expect_equal(length(top10longest), 10L)
    expect_equal(sum(top10longest), 80513L)
  }
)

test_that(
  "max_val_int", {
    expect_equal(length(max_val_int), 1L)
    expect_equal(sum(max_val_int * top10longest), 777997119L)
  }
)

test_that(
  "even_odd_avg_len", {
    expect_equal(length(even_odd_avg_len), 2L)
    expect_equal(even_odd_avg_len[2] / even_odd_avg_len[1], 1.160139, 
                 tolerance = 1e-3)
  }
)

test_that(
  "even_odd_sd_len", {
    expect_equal(length(even_odd_sd_len), 2L)
    expect_equal(even_odd_sd_len[2] / even_odd_sd_len[1], 1.046134, 
                 tolerance = 1e-3)
  }
)

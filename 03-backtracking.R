# Task 3 ---------------------------------------------------------------
# Investigating "backtracking" in sequences

# 1) Filter collatz_df to retain starting integers

a_backtracking <- function(seq, start) {
  value_less_than_start <- FALSE
  
  for (value in seq) {
    if (value < start) {
      value_less_than_start <- TRUE
    }
    if (value_less_than_start & value > start) {
      return(TRUE)
    }
    if (value == 1) {
      break
    }
  }
  return(FALSE)
}

backtracks_df <- collatz_df %>%
  filter(pmap_lgl(list(seq, start), a_backtracking))

print(backtracks_df)
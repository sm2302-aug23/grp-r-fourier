# Task 3 ---------------------------------------------------------------
#Investigating "backtracking" in sequences

# 1) Filter collatz_df to retain starting integers

a_backtracking <- function(seq) {
  min_val <- Inf
  for (num in seq) {
    if (num < min_val) {
      min_val <- num
    } else if (num > min_val) {
      return(TRUE)
    }
  }
  return(FALSE)
}

backtracks_df <- collatz_df %>%
  filter(sapply(seq, a_backtracking))

print(backtracks_df)
head(backtracks_df)
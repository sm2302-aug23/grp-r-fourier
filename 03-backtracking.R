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


# 2) Most frequently occuring number of time sequences go above their starting integer

max_count <- function(seq,start) {
  value_greater_than_start <- FALSE
  count <- 0
  
  for (value in seq) {
    if(value > start) {
      value_greater_than_start <- TRUE
      count <- count + 1
    }
    if(value == 1) {
      break
    }
  }
  return(count)
}

count_df <- backtracks_df %>%
  mutate(count = pmap_int(list(seq, start), max_count))

my_mode <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

mode_backtrack <- my_mode(count_df$count)

print(mode_backtrack)
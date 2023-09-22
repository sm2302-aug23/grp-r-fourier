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


# 2) Most frequently occurring number of time sequences go above their starting integer

max_count <- function(seq, start) {
  sum(seq > start)
}

count_df <- backtracks_df %>%
  mutate(count=pmap_int(list(seq,start),max_count))

mode_backtrack <- count_df %>%
  group_by(count) %>%
  summarise(seq_count = n()) %>%
  filter(seq_count == max(seq_count)) %>%
  pull(count)

mode_backtrack


# 3) Maximum value reached after the first backtrack

max_val_reached <- function(seq, start) {
  value_less_than_start <- FALSE
  max_val <- 0
  
  for (value in seq) {
    if (value < start) {
      value_less_than_start <- TRUE
    }
    if (value_less_than_start & value > start) {
      max_val <- max(max_val,value)
    }
    if (value == 1) {
      break
    }
  }
  return(max_val)
}

max_after_backtrack <- backtracks_df %>%
  mutate(max_val_reached = pmap_int(list(seq, start), max_val_reached)) %>%
  select(max_val_reached) 

max_after_backtrack <- t(max_after_backtrack)

max_after_backtrack

# 4) Frequency counts for even and odd backtracking integers

even_odd_backtrack <- backtracks_df %>%
  count(parity, name = "count") %>%
  select(count)

even_odd_backtrack <- t(even_odd_backtrack)

even_odd_backtrack

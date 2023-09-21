# Task 1 -----------------------------------------------
# Generating the Collatz Conjecture

library(tidyverse)
library(ggplot2)

gen_collatz <- function(n) {
  if(n <= 0 || !is.integer(n)) {
    stop("n must be a positive integer")
  }
  
  seq <- c(n)
  
  while(n != 1) {
    if(n %% 2 == 0) {
      n <- n/2
    } else {
      n <- 3*n + 1
    }
    seq <- c(seq, n)
  }
  
  return(seq)
}

collatz_df <- tibble(
  start = 1:10000,
  seq = lapply(1:10000, function(n)
    if (n == 1) c(1)
    else gen_collatz(n)),
  length = map_dbl(seq, length),
  parity = ifelse(start %% 2 ==0, 'Even', 'Odd'),
  max_val = map_dbl(seq, max)
)

collatz_df

# Task 2 -----------------------------------------------
# Exploratory data analysis

# 1) Top 10 longest starting integers

top10longest <- collatz_df %>%
  arrange(desc(length)) %>%
  slice_head(n = 10) %>%
  select(start)
  
top10longest <- as_tibble(t(top10longest))

top10longest

# 2) Starting integer with maximum sequence value

max_val_int <- collatz_df %>%
  arrange(desc(max_val)) %>%
  head(1) %>%
  select(start)

max_val_int <- t(max_val_int)

max_val_int

# 3) Average length and standard deviation of sequences for even and odd

even_odd_avg_len <- collatz_df %>%
  group_by(parity) %>%
  summarise(avg = mean(length)) %>%
  arrange(avg) %>%
  select(avg)

even_odd_avg_len <- t(even_odd_avg_len)

even_odd_avg_len


even_odd_sd_len <- collatz_df %>%
  group_by(parity) %>%
  summarise(sd = sd(length)) %>%
  arrange(sd) %>%
  select(sd)

even_odd_sd_len <- t(even_odd_sd_len)

even_odd_sd_len

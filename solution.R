# Task 1 -----------------------------------------------
# Generating the Collatz Conjecture

library(tidyverse)
library(ggplot2)

gen_collatz <- function(n) {
  if(n <= 0) {
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
  seq = lapply(1:10000, gen_collatz),
  length = map_dbl(seq, length),
  max_val = map_dbl(seq, max)
)

collatz_df

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
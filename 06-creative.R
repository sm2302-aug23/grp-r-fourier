library(tidyverse)

gen_collatz_length <- function(n) {
 if (n == 1) return(1)
 length <- 1
 while (n !=1) {
 if (n %% 2 == 0) {
  n <- n/2
} else {
 n <- 3* n +1
} 
length <- length +1 
}
return(length)
}

collatz_length_df <- tibble(
start = 1:100,
length = map_dbl(start, gen_collatz_length),
parity = ifelse (start %%2==0, 'Even', 'Odd'))

ggplot(collatz_length_df, aes(x= start, y= length, color = parity)) +
geom_line() +
labs(title = "Collatz Conjecture Sequence Length", 
        x= "Starting Integer",
        y = "Sequence Length",
        color = "Parity"
 ) +

theme_minimal()

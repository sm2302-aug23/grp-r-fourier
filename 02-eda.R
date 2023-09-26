# Task 2 -----------------------------------------------
# Exploratory Data Analysis

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
# Task 4 -----------------------------------------------
# Visualisation

library(ggplot2)

# 1) Creating a scatterplot of all the sequence lengths

ggplot(data = backtracks_df,
       aes(x = start,
           y = length)
) +
  geom_point() +  
  geom_point(data = head(backtracks_df, 10), 
             aes(colour = "red"),
             size = 3) +
  labs(
    title = "Collatz Conjecture",
    x = "Starting Integer",
    y = "Length of Sequence",
    colour = "Top 10 Starting Integer") 


# Add limits to see the top 10 starting integers more clearly

ggplot(data = backtracks_df,
       aes(x = start,
           y = length)
) +
  geom_point() +  
  geom_point(data = head(backtracks_df, 10), 
             aes(colour = "red"),
             size = 3) +
  labs(
    title = "Collatz Conjecture",
    x = "Starting Integer",
    y = "Length of Sequence",
    colour = "Top 10 Starting Integer") +
  scale_x_continuous(
    limits = c(0, 2000)) +
  scale_y_continuous(
    limits = c(0,100))


# 2) Creating a scatterplot of the highest value reached in the sequence

max_after_backtrack_df <- backtracks_df %>%
  mutate(max_val_reached = pmap_dbl(list(seq, start), max_val_reached))

ggplot(data = max_after_backtrack_df,
       aes(x = start,
           y = max_val_reached)) +
  geom_point() +  
  geom_point(data = head(max_after_backtrack_df, 10),
             aes(colour = factor(start)), 
             size = 5) +
  labs(
    title = "The Highest Value Reached in the Sequence",
    x = "Starting Integer",
    y = "Maximum Value Reached",
    colour = "Top 10 Starting Integer") 


# Add limits to see the top 10 starting integers more clearly

ggplot(data = max_after_backtrack_df,
       aes(x = start,
           y = max_val_reached)) +
  geom_point() +  
  geom_point(data = head(max_after_backtrack_df, 10),
             aes(colour = factor(start)), 
             size = 5) +
  labs(
    title = "The Highest Value Reached in the Sequence",
    x = "Starting Integer",
    y = "Maximum Value Reached",
    colour = "Top 10 Starting Integer") +
  scale_x_continuous(
    limits = c(0, 500)
  ) +
  scale_y_continuous(
    limits = c(0, 500)
  )


# 3) Creating a boxplot comparing the distributions of sequence lengths for even and odd integers

ggplot(data = backtracks_df) +
  geom_boxplot(aes(x = parity,
                   y = length)) +
  labs(
    title = "Distributions of Sequence Lengths",
    x = "Parity of Starting Integer",
    y = "Length of Sequence")
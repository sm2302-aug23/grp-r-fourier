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
    title = "The Collatz Conjecture",
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
    title = "The Collatz Conjecture",
    x = "Starting Integer",
    y = "Length of Sequence",
    colour = "Top 10 Starting Integer") +
  scale_x_continuous(
    limits = c(0, 2000)) +
  scale_y_continuous(
    limits = c(0,100))
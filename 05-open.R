# Task 5 ----------------------------------------------------------------------

# What is the most frequent integer that appears in all the sequences combined, 
# excluding the number 1?

# Most frequent integer other than 1:

separated_seq <- collatz_df %>%
  unnest(seq) %>%
  filter(seq != 1) %>%
  arrange(seq)
separated_seq

# Combine all of the numbers in the sequences 
# by seperating numbers in c() with unnest()

most_frequent <- separated_seq %>%
  count(seq) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

most_frequent 

# Filter n to be > 1 to exclude 1

# Results found:

# 1) The most frequently appearing integer other than 1 is 2
# 2) The top 10 are mostly multiples of 2 and 5
# 3) The sequences correlate with each other

# Even starting integer, e.g. 6
collatz_df$seq[6]

# Which yields the sequence:
c(6, 3, 10, 5, 16, 8, 4, 2, 1)

# Notice how as it reaches 1, the last four numbers before 1 are:
# 1) multiples of 2, and 
# 2) powers of 2
# This shows that the Collatz process naturally reduces powers of 2 
# because of n/2 if the previous integer is even

# Odd starting integer, e.g. 21
collatz_df$seq[21]

# Which yields the sequence:
c(21, 64, 32, 16, 8, 4, 2, 1)

# Although it starts as an odd number, the next term is 3*21 + 1 which is 64, 
# an even number and it is a multiple and a power of 2,
# so the next terms will continue to be n/2 until the sequence reaches 1.

# Sequences that correlate with each other:

# Most starting integers will have same numbers in their sequence

# Another even starting integer, e.g. 52
collatz_df$seq[52]

# Which yields the sequence:
c(52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)

# In relation with the even starting integer 6 above:
# we can see that the 6th term in starting integer 52 is the same as 
# the 3rd term in starting integer 6, which is 10
# so, the next sequence will be the same, 
# 10/2 is 5, then 3*5 + 1 is 16, 16/2 = 8, 8/2 = 4, 4/2 = 2, and lastly 2/2 = 1.

# Another odd starting integer, e.g. 113
collatz_df$seq[113]

# Which yields the sequence:
c(113, 340, 170, 85, 256, 128, 64, 32, 16, 8, 4, 2, 1)

# This sequence is related to the odd starting integer 21 above:
# the 7th term in starting integer 113 is the same as 
# the 2nd term in starting integer 21, which is 64,
# so the next sequence, again, will be the same:
c(32, 16, 8, 4, 2, 1)

# That is why the top 5 most frequent integers excluding 1 are
c(2, 4, 8, 16)
# because of their divisibility by 2 and their nature as powers of 2.

# Interestingly, starting integers of powers of 2 have the 
# shortest sequence lengths among their neighboring starting integers

in10start <- collatz_df %>%
  select(start, length) %>%
  slice(1:10) %>%
  arrange(length)

print(in10start)

# In the first 10 starting integers, the top 4 shortest lengths have 
# starting integers of powers of 2, which are 2, 4 and 8 (excluding 1)

in20start <- collatz_df %>%
  select(start, length) %>%
  slice(11:20) %>%
  arrange(length)

print(in20start)

# 16, a power of 2 (2^4) has the shortest length of 5 
# among starting integers 11 to 20.

in30start <- collatz_df %>%
  select(start, length) %>%
  slice(31:40) %>%
  arrange(length)

print(in30start)

# 32, a power of 2 (2^5) has the shortest length of 6 
# among starting integers 31 to 40.

in60start <- collatz_df %>%
  select(start, length) %>%
  slice(61:70) %>%
  arrange(length)

print(in60start)

# 64, a power of 2 (2^6) has the shortest length of 7
# among starting integers 61 to 70.

# This is because the next terms for 2^n starting integers are all even
# and only need to be divided by 2 until the sequences reach 1,
# unlike the other starting integers that have odd numbers in their sequences.

# So, 2 is the most frequent integer (other than 1) that appears 
# in all the sequences combined because it is the second last integer to be 
# in a sequence before reaching 1, as 2/2 = 1.

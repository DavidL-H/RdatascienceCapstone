# David Lennox-Hvenekilde
# 220326

# Capstone project
# Part 1
# Getting and cleaning the data

# This project encompasses:
# Natural language processing and text mining


# Read the first data
# We will randomly sample a 10 000 line subset of each of the data files
# and save these in new files, to use in our future models

# Set the working directory and open connection to file, then read it
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
con <- file("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
en_US.twitter <- readLines(con)

# Random sampling
set.seed(98631)
random_set <- sample(1:length(en_US.twitter), size = 10000, replace = FALSE)

# Read the random samples lines into a new vector
en_US.twitter_random <- en_US.twitter[random_set]

# Save the random subset in a new file
save(en_US.twitter_random, file = "./ReducedData/en_US.twitter_random_10k.txt")
close(con)


# Quiz 1
# Read in the two other english data sets
con <- file("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
en_US.blogs <- readLines(con)
close(con)

con <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt")
en_US.news <- readLines(con)
close(con)

# How do we find the longest line in each of the 3 texts?

which.max(nchar(en_US.twitter))
which.max(nchar(en_US.blogs))
which.max(nchar(en_US.news))

# Question 4
# In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
length(grep("love", en_US.twitter))/length(grep("hate", en_US.twitter))

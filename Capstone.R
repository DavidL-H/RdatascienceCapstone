# David Lennox-Hvenekilde
# 220326

# Capstone project
# Part 1
# Getting the data

# This project encompasses:
# Natural language processing and text mining


# Read the first data
# We will use rbinom to sample a random subset of the data and save this for later use
con <- file("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
n(con)

en_US.twitter_length <- length(readLines(con))

set.seed(98631)
random_set <- sample(1:en_US.twitter_length, siz = 100, replace = FALSE)

en_US.twitter_random <- c()
for (i in random_set){
      en_US.twitter_random <- c(en_US.twitter_random,readLines(con,i))
}






2+2









close(con)
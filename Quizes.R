
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

# Question 5
# The one tweet in the en_US twitter data set that matches the word "biostats" says what?
en_US.twitter[grep("biostats", en_US.twitter)]

en_US.twitter[grep("A computer once beat me at chess, but it was no match for me at kickboxing", en_US.twitter)]

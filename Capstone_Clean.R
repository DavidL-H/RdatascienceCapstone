# David Lennox-Hvenekilde
# 220326

# Capstone project

# Part 1##############################################
# Getting and cleaning the data

# This project encompasses:
# Natural language processing and text mining


# Read the first data
# We will randomly sample a 10 000 line subset of each of the data files
# and save these in new files, to use in our future models

# Set the working directory and open connection to file, then read it
setwd("C:/Users/dal/Biosyntia/Admin - David Lennox-Hvenekilde/Bioinformatics and data science/Courses/Data Science in R/Capstone/RdatascienceCapstone")
con <- file("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
en_US.twitter <- readLines(con)
close(con)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
con <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt")
en_US.news <- readLines(con)
close(con)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
con <- file("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
en_US.blogs <- readLines(con)
close(con)

en_US.twitter[1]
length(en_US.twitter)
en_US.blogs[1]
en_US.news[1]

# Random sampling
set.seed(98631)
random_set1 <- sample(1:length(en_US.twitter), size = 0.20*length(en_US.twitter), replace = FALSE)
random_set2 <- sample(1:length(en_US.news), size = 0.20*length(en_US.news), replace = FALSE)
random_set3 <- sample(1:length(en_US.blogs), size = 0.20*length(en_US.blogs), replace = FALSE)

# Read the random samples lines into a new vector
library(tibble)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

en_US.twitter_random <- tibble(txt = en_US.twitter[random_set1])
en_US.news_random<- tibble(txt = en_US.news[random_set2])
en_US.blogs_random <- tibble(txt = en_US.blogs[random_set3])




#en_US.twitter_random_tidy_token <- en_US.twitter_random_tidy %>% unnest_tokens(word, txt)

# Task 2 #######################
# Exploratory data analysis

# Create a tidy long-format dataframe with all text sources
en_US.twitter_random$Src <- "en_US.twitter"
en_US.twitter_random$Line <- 1:nrow(en_US.twitter_random)
en_US.news_random$Src <- "en_US.news"
en_US.news_random$Line <- 1:nrow(en_US.news_random)
en_US.blogs_random$Src <- "en_US.blogs"
en_US.blogs_random$Line <- 1:nrow(en_US.blogs_random)

# en_US.all <- rbind(en_US.twitter_random, en_US.news_random, en_US.blogs_random)
# write.csv(en_US.all, "./ReducedData/en_US.allv2.csv")
# class(en_US.all)
# head(en_US.all)






# TEST DATA
set.seed(98631)
random_set1 <- sample(1:length(en_US.twitter), size = 0.20*length(en_US.twitter), replace = FALSE)
random_set2 <- sample(1:length(en_US.news), size = 0.20*length(en_US.news), replace = FALSE)
random_set3 <- sample(1:length(en_US.blogs), size = 0.20*length(en_US.blogs), replace = FALSE)
en_US.twitter_random <- tibble(txt = en_US.twitter[!(1:length(en_US.twitter)%in%random_set1)])
en_US.news_random<- tibble(txt = en_US.news[!(1:length(en_US.news)%in%random_set1)])
en_US.blogs_random <- tibble(txt = en_US.blogs[!(1:length(en_US.blogs)%in%random_set3)])

en_US.twitter_random$Src <- "en_US.twitter"
en_US.twitter_random$Line <- 1:nrow(en_US.twitter_random)
en_US.news_random$Src <- "en_US.news"
en_US.news_random$Line <- 1:nrow(en_US.news_random)
en_US.blogs_random$Src <- "en_US.blogs"
en_US.blogs_random$Line <- 1:nrow(en_US.blogs_random)

en_US.all <- rbind(en_US.twitter_random, en_US.news_random, en_US.blogs_random)
write.csv(en_US.all, "./ReducedData/en_US.all.TEST.csv")
en_US.all.token <- en_US.all %>% unnest_tokens(word, txt)

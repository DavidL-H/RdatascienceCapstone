# David Lennox-Hvenekilde
# 220329

# Capstone project
# Part 2 - Exploratory data analysis
# Approach inspired by: https://www.tidytextmining.com/tidytext.html

library(tibble)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data in
en_US.all <- read.csv("./ReducedData/en_US.allv2.csv", row.names = 1)

# Tokenize words
en_US.all.token <- en_US.all %>% unnest_tokens(word, txt)

# Make a smaller dataset of non-stopwords
data("stop_words")
en_US.all.nonstop <- en_US.all.token %>% anti_join(stop_words)


# Count words
en_US.all.wordcount <- en_US.all.token %>% count(word, sort = TRUE) 
en_US.all.nonstop.wordcount <- en_US.all.nonstop %>% count(word, sort = TRUE) 

top5all <- en_US.all.wordcount[1:5,]
top5all$freq <- top5all$n/nrow(en_US.all.token)

top5nonstop <- en_US.all.nonstop.wordcount[1:5,]
top5nonstop$freq <- top5nonstop$n/nrow(en_US.all.nonstop)




 # Compare frequencies based on source (Src)
# We use the dataset WITHOUT stopwords as it is more interesting to compare these
frequency <- bind_rows(mutate(en_US.all.nonstop[en_US.all.nonstop$Src == "en_US.twitter",], source = "en_US.twitter"),
                       mutate(en_US.all.nonstop[en_US.all.nonstop$Src == "en_US.news",], source = "en_US.news"), 
                       mutate(en_US.all.nonstop[en_US.all.nonstop$Src == "en_US.blogs",], source = "en_US.blogs")) %>% 
      mutate(word = str_extract(word, "[a-z']+")) %>%
      count(source, word) %>%
      group_by(source) %>%
      mutate(proportion = n / sum(n)) %>% 
      select(-n) %>% 
      pivot_wider(names_from = source, values_from = proportion) %>%
      pivot_longer(`en_US.news`:`en_US.blogs`,
                   names_to = "source", values_to = "proportion")


# Show a barplot of the top 20 words, and thier frequencies in the data
p<-ggplot(data=en_US.all.nonstop.wordcount[1:20,], aes(x=reorder(word, -n), y=n)) +
      geom_bar(stat="identity") +
      labs(x = "Word",
           y = "Occurance (n)")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

# Histogram of word frequency
hist(log10(en_US.all.nonstop.wordcount$n),breaks = 15)

# Comparing word frequency from the 3 different sources
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `en_US.twitter`, 
                      color = abs(`en_US.twitter` - proportion))) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), 
                           low = "darkslategray4", high = "gray75") +
      facet_wrap(~source, ncol = 2) +
      theme(legend.position="none") +
      labs(y = "en_US.twitter", x = NULL)




# Check n-grams
en_US.all.2gram <- en_US.all %>% 
      unnest_tokens(bigram, txt, token = "ngrams", n = 2) %>% 
      separate(bigram, c("word1", "word2"), sep = " ")
en_US.all.3gram <- en_US.all %>% 
      unnest_tokens(trigram, txt, token = "ngrams", n = 3) %>% 
      separate(trigram, c("word1", "word2", "word3"), sep = " ")
en_US.all.4gram <- en_US.all %>%
      unnest_tokens(quadgram, txt, token = "ngrams", n = 4) %>%
      separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

# Takes too long to run
#en_US.all.2gram.freq <- en_US.all.2gram %>% count(bigram, sort = TRUE) 


# Model building

write.csv(en_US.all.2gram, "./ReducedData/enUSall2gram.csv")
write.csv(en_US.all.3gram, "./ReducedData/enUSall3gram.csv")
write.csv(en_US.all.4gram, "./ReducedData/enUSall4gram.csv")

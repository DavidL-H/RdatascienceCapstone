# Lets try out the n-gram model using a full data-set
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# en_US.all.2gram <- read.csv("./ReducedData/enUSall2gram.csv")
# en_US.all.3gram <- read.csv("./ReducedData/enUSall3gram.csv")
# en_US.all.4gram <- read.csv("./ReducedData/enUSall4gram.csv")

library(tibble)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Use n-grams to predict
predict.next.word <- function(sentence){
      # tokenize sentence
      if (str_count(sentence, '\\w+') > 1){
            sentence <- data.frame(txt = sentence)
            sentence <- sentence %>% unnest_tokens(word, txt)
            sentence <- sentence[(nrow(sentence)-2):nrow(sentence),1] 
      }
      
      predict
      if (length(sentence) == 3){
            gramhit <- en_US.all.4gram %>% filter(word1 == sentence[1], word2 == sentence[2], word3 == sentence[3]) %>%
                  count(word4) %>%
                  slice_max(n,n=3, with_ties = FALSE)
            if (is.na(gramhit[1,1])){
                  sentence <- sentence[-1]
            }
      }
      print(sentence)
      
      if ((length(sentence) == 2)){
            gramhit <- en_US.all.3gram %>% filter(word1 == sentence[1], word2 == sentence[2]) %>% 
                  count(word3) %>%
                  slice_max(n, n=3, with_ties = FALSE)
            
            if (is.na(gramhit[1,1])){
                  sentence <- sentence[-1]
            }
      }
      
      if (length(sentence) == 1){
            gramhit <- en_US.all.2gram %>% filter(word1 == sentence[1]) %>% 
                  count(word2) %>%
                  slice_max(n, n=3, with_ties = FALSE)
}
return(as.data.frame(gramhit))
}

start_time <- Sys.time()
predict.next.word("This model is fast but it take up too much RAM, so I will")
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
predict.next.word("Then we can")
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
predict.next.word("should")
end_time <- Sys.time()
end_time - start_time

predict.next.word("This doesn't work for words not in the corpora")

# The model takes way too much RAM
object.size(en_US.all.2gram)
object.size(en_US.all.3gram)
object.size(en_US.all.4gram)
# The total ram used by these objects is 164+213+259 = 636 MB


# Need to increase training set size.
# Back-off model is already implemented intuitively
# Data needs to me "smoothed" in order to account of unobserved ngrams

predict.next.word("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
predict.next.word("You're the reason why I smile everyday. Can you follow me please? It would mean the")
predict.next.word("Hey sunshine, can you follow me and make me the")
predict.next.word("Very early observations on the Bills game: Offense still struggling but the")
predict.next.word("Go on a romantic date at the")
predict.next.word("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")

# NOTES
# Can data be reduced by deleting repetive ngrams and just giving an overall frequency?
# Only give the ngram frequency with top 3 hits, remove the rest from the model.

# The data additionally needs to be smoothed. One option is Good-Turing discounting 
# https://rpubs.com/leomak/TextPrediction_KBO_Katz_Good-Turing


# Reduction of model size
# en_US.all.2gram <- read.csv("./ReducedData/enUSall2gram.csv")
# en_US.all.2gram.temp <- en_US.all.2gram
# en_US.all.2gram.freq <- data.frame(word1 = NA,
#                                    hit1 = NA,
#                                    hit1_n = NA,
#                                    hit2 = NA,
#                                    hit2_n = NA,
#                                    hit3 = NA,
#                                    hit3_n = NA,
#                                    ngram_n = NA)
# colnames(en_US.all.2gram.freq)
# nrow(en_US.all.2gram.temp)>1
# i=1
# while (nrow(en_US.all.2gram.temp)>1 & !is.na(en_US.all.2gram.temp$word1[1])){
#       word <- en_US.all.2gram.temp$word1[1]
#       gramhit <- en_US.all.2gram.temp %>% filter(word1 == word) %>%
#             count(word2) %>%
#             slice_max(n, n=3, with_ties = FALSE)
#       print(i)
#       en_US.all.2gram.freq[i,]$word1 <- word
#       en_US.all.2gram.freq[i,]$hit1 <- gramhit$word2[1]
#       en_US.all.2gram.freq[i,]$hit1_n <- gramhit$n[1]
#       en_US.all.2gram.freq[i,]$hit2 <- gramhit$word2[2]
#       en_US.all.2gram.freq[i,]$hit2_n <- gramhit$n[2]
#       en_US.all.2gram.freq[i,]$hit3 <- gramhit$word2[3]
#       en_US.all.2gram.freq[i,]$hit3_n <- gramhit$n[3]
#
#
#       # Reduce the temporary dataframe to avoid repetition
#       lenBefore <- nrow(en_US.all.2gram.temp)
#
#       en_US.all.2gram.temp <- en_US.all.2gram.temp[en_US.all.2gram.temp$word1!=word,]
#
#       lenAfter <- nrow(en_US.all.2gram.temp)
#       en_US.all.2gram.freq[i,]$ngram_n <- lenBefore-lenAfter
#
#       print(lenAfter)
#       i = i + 1
# }
# write.csv(en_US.all.2gram.freq, "./ReducedData/en_US.all.2gram.freq.csv")
# rm(en_US.all.2gram)
# rm(en_US.all.2gram.temp)
# rm(en_US.all.2gram.freq)
# en_US.all.2gram.freq <- read.csv(file = "./ReducedData/en_US.all.2gram.freq.csv")
# sum(en_US.all.2gram.freq$ngram_n)

# 3gram reduction# Reduction of model size
  en_US.all.3gram <- read.csv("./ReducedData/enUSall3gram.csv")
  en_US.all.freq.temp <- read.csv("./ReducedData/en_US.all.3gram.freq.csv")
  en_US.all.freq.temp2 <- read.csv("./ReducedData/en_US.all.3gram.freq2.csv")
  en_US.all.3gram$word1 <- paste(en_US.all.3gram$word1, en_US.all.3gram$word2, sep = " ")
  
  en_US.all.3gram <- en_US.all.3gram[!(en_US.all.3gram$word1 %in% en_US.all.freq.temp$word1) & !(en_US.all.3gram$word1 %in% en_US.all.freq.temp2$word1),]
  
  # Take 20%
  set.seed(1234)
  
  
  # Work more on the 3gram model based on freq tables ################################################
  # Here we subset the 3gram data based on the already processed frequency data
  # We remove the 3gram that we have already counted in en_US.all.freq.temp-en_US.all.freq.temp3
  en_US.all.freq.temp <- read.csv("./ReducedData/en_US.all.3gram.freq.csv")
  en_US.all.freq.temp2 <- read.csv("./ReducedData/en_US.all.3gram.freq2.csv")
  en_US.all.freq.temp3 <- read.csv("./ReducedData/en_US.all.3gram.freq3.csv")
  
  en_US.all.3gram.freq.master <- rbind(en_US.all.freq.temp,en_US.all.freq.temp2,en_US.all.freq.temp3)
  en_US.all.3gram <- read.csv("./ReducedData/enUSall3gram.csv")
  en_US.all.3gram$word1 <- paste(en_US.all.3gram$word1, en_US.all.3gram$word2, sep = " ")
  
  gramhit.temp <- en_US.all.3gram[!(en_US.all.3gram$word1 %in% en_US.all.3gram.freq.master$word1),]
  gramhit <- gramhit.temp %>% count(word1)
  # Now lets subset the data to only get those 3 grams appearing more than 2 time
  nrow(gramhit[gramhit$n>3,])
  hist(gramhit$n)
  
  
  en_US.all.3gram <- en_US.all.3gram[en_US.all.3gram$word1 %in% gramhit[gramhit$n>3,]$word1,]
  
  
 # en_US.all.3gram.temp <-  en_US.all.3gram[sample(1:nrow(en_US.all.3gram), size = 0.25*nrow(en_US.all.3gram), replace = FALSE),]
  en_US.all.3gram.temp <- en_US.all.3gram
  rm(en_US.all.3gram)
  rm(en_US.all.3gram.freq)
  rm(en_US.all.freq.temp)
  rm(en_US.all.freq.temp2)
  en_US.all.3gram.freq <- data.frame(word1 = NA,
                                     hit1 = NA,
                                     hit1_n = NA,
                                     hit2 = NA,
                                     hit2_n = NA,
                                     hit3 = NA,
                                   hit3_n = NA,
                                    ngram_n = NA)
 colnames(en_US.all.3gram.freq)
 nrow(en_US.all.3gram.temp)>1
 i=1
 while (nrow(en_US.all.3gram.temp)>1 & !is.na(en_US.all.3gram.temp[1,]$word1)){
       word <- en_US.all.3gram.temp[1,]$word1
       gramhit.temp <- en_US.all.3gram.temp[en_US.all.3gram.temp$word1==word,]
       gramhit <- gramhit.temp %>% filter(word1 == word) %>%
             count(word3) %>%
             slice_max(n, n=3, with_ties = FALSE)
       print(i)
       en_US.all.3gram.freq[i,]$word1 <- word
       en_US.all.3gram.freq[i,]$hit1 <- gramhit$word3[1]
       en_US.all.3gram.freq[i,]$hit1_n <- gramhit$n[1]
       en_US.all.3gram.freq[i,]$hit2 <- gramhit$word3[2]
       en_US.all.3gram.freq[i,]$hit2_n <- gramhit$n[2]
       en_US.all.3gram.freq[i,]$hit3 <- gramhit$word3[3]
       en_US.all.3gram.freq[i,]$hit3_n <- gramhit$n[3]


       # Reduce the temporary dataframe to avoid repetition
       lenBefore <- nrow(en_US.all.3gram.temp)

       en_US.all.3gram.temp <- en_US.all.3gram.temp[en_US.all.3gram.temp$word1!=word,]
 
        lenAfter <- nrow(en_US.all.3gram.temp)
        en_US.all.3gram.freq[i,]$ngram_n <- lenBefore-lenAfter
 
       print(lenAfter)
        i = i + 1
  }
  write.csv(en_US.all.3gram.freq, "./ReducedData/en_US.all.3gram.freq4.csv")
  rm(en_US.all.3gram.temp)
  rm(en_US.all.3gram.freq)

 # 4gram reduction

 en_US.all.4gram.temp <- read.csv("./ReducedData/enUSall4gram.csv")
 en_US.all.4gram.temp <- en_US.all.4gram[sample(1:nrow(en_US.all.4gram.temp), size = 0.25*nrow(en_US.all.4gram.temp), replace = FALSE),]
 en_US.all.4gram.temp$word1 <- paste(en_US.all.4gram.temp$word1, en_US.all.4gram.temp$word2, en_US.all.4gram.temp$word3,  sep = " ")
 en_US.all.4gram.freq <- data.frame(word1 = NA,
                                    hit1 = NA,
                                    hit1_n = NA,
                                    hit2 = NA,
                                    hit2_n = NA,
                                    hit3 = NA,
                                    hit3_n = NA,
                                    ngram_n = NA)
 colnames(en_US.all.4gram.freq)
 nrow(en_US.all.4gram.temp)>1
 i=1
 while (i<200000 & !is.na(en_US.all.4gram.temp[1,]$word1)){
       word <- en_US.all.4gram.temp[1,]$word1
       gramhit <- en_US.all.4gram.temp %>% filter(word1 == word) %>%
             count(word4) %>%
             slice_max(n, n=3, with_ties = FALSE)
       print(i)
       en_US.all.4gram.freq[i,]$word1 <- word
       en_US.all.4gram.freq[i,]$hit1 <- gramhit$word4[1]
       en_US.all.4gram.freq[i,]$hit1_n <- gramhit$n[1]
       en_US.all.4gram.freq[i,]$hit2 <- gramhit$word4[2]
       en_US.all.4gram.freq[i,]$hit2_n <- gramhit$n[2]
       en_US.all.4gram.freq[i,]$hit3 <- gramhit$word4[3]
       en_US.all.4gram.freq[i,]$hit3_n <- gramhit$n[3]
       
       
       # Reduce the temporary dataframe to avoid repetition
       lenBefore <- nrow(en_US.all.4gram.temp)
       
       en_US.all.4gram.temp <- en_US.all.4gram.temp[en_US.all.4gram.temp$word1!=word,]
       
       lenAfter <- nrow(en_US.all.4gram.temp)
       en_US.all.4gram.freq[i,]$ngram_n <- lenBefore-lenAfter
       
       print(lenAfter)
       i = i + 1
 }
 write.csv(en_US.all.4gram.freq, "./ReducedData/en_US.all.4gram.freq.csv")
 rm(en_US.all.4gram.temp)
 rm(en_US.all.4gram.freq)
 
 
 
 

 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)

en_US.all.2gram.freq <- read.csv("./ReducedData/en_US.all.2gram.freq.csv")
en_US.all.3gram.freq <- read.csv("./ReducedData/en_US.all.3gram.freq.csv")
en_US.all.3gram.freq2 <- read.csv("./ReducedData/en_US.all.3gram.freq2.csv")
en_US.all.3gram.freq3 <- read.csv("./ReducedData/en_US.all.3gram.freq3.csv")
en_US.all.3gram.freq4 <- read.csv("./ReducedData/en_US.all.3gram.freq4.csv")

en_US.all.3gram.freq <- rbind(en_US.all.3gram.freq,en_US.all.3gram.freq2,en_US.all.3gram.freq3,en_US.all.3gram.freq4)
en_US.all.3gram.freq <- en_US.all.3gram.freq %>% filter(ngram_n > 3)

en_US.all.2gram.freq <- en_US.all.2gram.freq[,-1]
en_US.all.3gram.freq <- en_US.all.3gram.freq[,-1]

# write.csv(en_US.all.3gram.freq, "./FinalModelData/en_US.all.3gram.freq.final.csv")
# write.csv(en_US.all.2gram.freq, "./FinalModelData/en_US.all.2gram.freq.final.csv")

predict.next.word <- function(sentence){
      # tokenize sentence
      if (str_count(sentence, '\\w+') > 1){
            sentence <- data.frame(txt = sentence)
            sentence <- sentence %>% unnest_tokens(word, txt)
            sentence <- sentence[(nrow(sentence)-1):nrow(sentence),1] 
      }
      # Use 3-grams in model to predict next word
      if ((length(sentence) == 2)){
            query <- paste(sentence[1], sentence[2],sep= " ")
            hit <- en_US.all.3gram.freq %>% filter(word1 == query)
            if (is.na(hit[1,1])){
                  sentence <- sentence[-1]
            }
      }
      # Backooff to 2-grams if a 3-gram is not in the model
      if ((length(sentence) == 1)){
            query <- sentence
            hit <- en_US.all.2gram.freq %>% filter(word1 == query)
            if (is.na(hit[1,1])){
                  hit <- data.frame(word1 = sentence, hit1 = "the", hit2 = "to", hit3 = "and")
            }
      }
      return(hit)
}






predict.next.word("Molecular biology")












# The model size is now only 28 MB
object.size(en_US.all.2gram.freq)
object.size(en_US.all.3gram.freq)


for (n in 1:nrow(en_US.all.3gram.Test)){
      word.prep <- predict.next.word(en_US.all.3gram.Test[n,]$sentence)
      if (!is.character(word.prep)){
            en_US.all.3gram.Test[n,]$hit1 <- word.prep$hit1
            en_US.all.3gram.Test[n,]$hit2 <- word.prep$hit2
            en_US.all.3gram.Test[n,]$hit3 <- word.prep$hit3
      }
      if (en_US.all.3gram.Test[n,]$word3 %in% c(en_US.all.3gram.Test[n,]$hit1,en_US.all.3gram.Test[n,]$hit2, en_US.all.3gram.Test[n,]$hit3)){
            en_US.all.3gram.Test[n,]$Correct <- TRUE
      }
}


write.csv(en_US.all.3gram.Test[1:10000,],file = "./ReducedData/en_US.all.3gram.Test.Model2.csv")
en_US.all.3gram.Test <- en_US.all.3gram.Test[1:10000,]

# How many predictions were true?
nrow(en_US.all.3gram.Test[en_US.all.3gram.Test$Correct == TRUE,])
# How many had no valid predictions?
nrow(en_US.all.3gram.Test[!is.na(en_US.all.3gram.Test$word1),])
object.size(en_US.all.3gram.freq)
object.size(en_US.all.2gram.freq)

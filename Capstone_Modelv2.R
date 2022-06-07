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

predict.next.word <- function(sentence){
      # tokenize sentence
      if (str_count(sentence, '\\w+') > 1){
            sentence <- data.frame(txt = sentence)
            sentence <- sentence %>% unnest_tokens(word, txt)
            sentence <- sentence[(nrow(sentence)-1):nrow(sentence),1] 
      }
      if ((length(sentence) == 2)){
            query <- paste(sentence[1], sentence[2],sep= " ")
            hit <- en_US.all.3gram.freq %>% filter(word1 == query)
            if (is.na(hit[1,1])){
                  sentence <- sentence[-1]
            }
      }
      if ((length(sentence) == 1)){
            query <- sentence
            hit <- en_US.all.2gram.freq %>% filter(word1 == query)
            if (is.na(hit[1,1])){
                  hit <- "Error: Ngrams not found"
            }
      }
      return(hit)
}

# The model size is now only 28 MB
object.size(en_US.all.2gram.freq)
object.size(en_US.all.3gram.freq)


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
predict.next.word("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predict.next.word("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
predict.next.word("I'd give anything to see arctic monkeys this")
predict.next.word("Talking to your mom has the same effect as a hug and helps reduce your")
predict.next.word("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
predict.next.word("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
predict.next.word("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
predict.next.word("Every inch of you is perfect from the bottom to the")
predict.next.word("I’m thankful my childhood was filled with imagination and bruises from playing")
predict.next.word("I like how the same people are in almost all of Adam Sandler's")

###########################################################################
# The model works in most cases, but needs a way to handle missing data.
# Let's test the accuary of the model on some of the training data.
TestDat <- read.csv("./ReducedData/en_US.all.TEST.csv")
TestDat <- TestDat[sample(1:nrow(TestDat), nrow(TestDat)*0.01),]
en_US.all.3gram.Test <- TestDat %>% 
      unnest_tokens(trigram, txt, token = "ngrams", n = 3) %>% 
      separate(trigram, c("word1", "word2", "word3"), sep = " ")

# Check model against test dataset:
en_US.all.3gram.Test$hit1 <- NA
en_US.all.3gram.Test$hit2 <- NA
en_US.all.3gram.Test$hit3 <- NA
en_US.all.3gram.Test$Correct <- FALSE



en_US.all.3gram.Test$sentence <- paste(en_US.all.3gram.Test$word1,en_US.all.3gram.Test$word2, sep = " ")

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

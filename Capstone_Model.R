# Lets try out the n-gram model using a full data-set

# Use n-grams to predict
predict.next.word <- function(sentence){
      # tokenize sentence
      if (str_count(sentence, '\\w+') > 1){
            sentence <- data.frame(txt = sentence)
            sentence <- sentence %>% unnest_tokens(word, txt)
            sentence <- sentence[(nrow(sentence)-2):nrow(sentence),1] 
      }
      
      # predict
      if (length(sentence) == 3){
            gramhit <- en_US.all.4gram %>% filter(word1 == sentence[1], word2 == sentence[2], word3 == sentence[3]) %>% 
                  count(word4) %>%
                  slice_max(n,n=3, with_ties = FALSE)
            if (is.na(gramhit[1,1])){
                  sentence <- sentence[-1]
            }
      }
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
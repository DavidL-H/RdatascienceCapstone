#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Library dependencies
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)

# Read the model data and define the prediction model function
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
en_US.all.2gram.freq <- read.csv("./FinalModelData/en_US.all.2gram.freq.final.csv")
en_US.all.3gram.freq <- read.csv("./FinalModelData/en_US.all.3gram.freq.final.csv")
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


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
      tableData = reactiveValues(d1 = as.data.frame(matrix(nrow=1,ncol = 9)))
      observeEvent(input$text, {
            beginning <- Sys.time()
            tableData$d1 <- predict.next.word(input$text)
            end <- Sys.time()
            
            # Generate text indicating prediction time
            output$readTime <- renderText({
                  print(paste("Prediction in:",round(end - beginning,digits = 4),"seconds"))
            })
            output$readTimeUI <- renderUI({
                  textOutput("readTime")
            })
            
            # Render action buttons based on the three options given by the prediction model
            # Option 1
            output$suggestion1UI <- renderUI({
                  actionButton("suggestion1", label = tableData$d1$hit1)
            })
            # Option 2
            output$suggestion2UI <- renderUI({
                  actionButton("suggestion2", label = tableData$d1$hit2)
            })
            # Option 3
            output$suggestion3UI <- renderUI({
                  actionButton("suggestion3", label = tableData$d1$hit3)
            })
            
      })
      output$PredictTable=renderTable({
            tableData$d1
      })
      
            # Now we can update the text input based on the chosen prediction option
            observeEvent(input$suggestion1,{
                  updateTextInput(session, "text", value = paste(input$text, tableData$d1$hit1))
            })
            observeEvent(input$suggestion2,{
                  updateTextInput(session, "text", value = paste(input$text, tableData$d1$hit2))
            })
            observeEvent(input$suggestion3,{
                  updateTextInput(session, "text", value = paste(input$text, tableData$d1$hit3))
            })
})

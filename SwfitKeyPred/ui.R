#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Swiftkey prediction assignment - Data Science Capstone project"),
    h4("David Lennox-Hvenekilde"),
    h4("220706"),
    tags$a(href="https://github.com/DavidL-H/RdatascienceCapstone", 
           "https://github.com/DavidL-H/RdatascienceCapstone"),
    p(""),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
              "Predictive text generation, using a so-called ",
              tags$a(href="https://aclanthology.org/D07-1090.pdf", 
                     "'stupid backoff model'."),
              "For more information about this app and the backend predictive model, please see the slide deck available here: ",
              tags$a(href="https://rpubs.com/Davidlh/DataScienceCapstone", 
                     "https://rpubs.com/Davidlh/DataScienceCapstone."),
              "Please note that this is a minimal viable product to show of the prediction algorithm. No bells or whistles included.",
              "Simply type below to start predicting, then choose one of 3 options if they fit, or keep typing, to continue predicting.",
              
              textInput("text", label = h3("Text input"), value = "", placeholder = "Please type text here to start prediction"),
              h4("Suggestions:"),
              p(uiOutput("suggestion1UI"),
              uiOutput("suggestion2UI"),
              uiOutput("suggestion3UI"),
              style="text-align: center;"),
              p(),
              tags$i(uiOutput("readTimeUI")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
              h3("Back-end frequency table"),
            tableOutput("PredictTable")
        )
    )
))

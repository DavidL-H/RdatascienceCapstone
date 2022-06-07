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
    titlePanel("Swiftkey prediction assignment - Data Science Captstone project"),
    h4("David Lennox-Hvenekilde"),
    h4("220706"),
    tags$a(href="https://github.com/DavidL-H/RdatascienceCapstone", 
           "https://github.com/DavidL-H/RdatascienceCapstone"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
              textInput("text", label = h3("Text input"), value = ""),
              p(uiOutput("suggestion1UI"),
              uiOutput("suggestion2UI"),
              uiOutput("suggestion3UI"),
              style="text-align: center;"),
              p(),
              tags$i(uiOutput("readTimeUI")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("PredictTable")
        )
    )
))

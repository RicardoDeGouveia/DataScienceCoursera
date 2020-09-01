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
    titlePanel("Predict Ozone level in airquality data with Temperature"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sliderTemp","What is the Temperature value?", min = 56, max = 97, value = 10),
            submitButton("Submit") # SubmitButton is handy for delayed reactivity where your server.R function includes calculation intensive algorithm
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot1"),
            h4("Predicted Ozone Level from the model (Red Circle):"),
            textOutput("pred1"),
            h6("A Quick Quide to Users: Enter the value of the Temperature with the above slider then press on Submit button")
        )
    )
))


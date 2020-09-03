### Data Science Capstone : Course Project
### ui.R file for the Shiny app
### Github repo : https://github.com/RicardoDeGouveia/DataScienceCoursera/tree/master/Data%20Science%20Capstone

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Coursera Data Science Capstone: Course Project",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Ricardo De Gouveia</strong>"),
                            br(),
                            HTML("<strong>Date: 02 September 2020</strong>"),
                            br(),
                            img(src = "C:/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/Data Science Capstone/Final Project/headers.png"),
                            # Sidebar
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("Enter a partially complete sentence to begin the next word prediction"),
                                    textInput("inputString", "Enter a partial sentence here",value = ""),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                                ),
                                mainPanel(
                                    h2("Predicted Next Word"),
                                    verbatimTextOutput("prediction"),
                                    strong("Sentence Input:"),
                                    tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                    textOutput('text1'),
                                    br(),
                                    strong("Note:"),
                                    tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                    textOutput('text2')
                                )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                                img(src = "C:/Users/Ricardo_dg18/Desktop/Certificados de COURSERA/Data Science Johns Hopkins University/DataScienceCoursera/Data Science Capstone/Final Project/headers.png"),
                                includeMarkdown("about.md")
                            )
                   )
)
)
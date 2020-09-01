#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

data("airquality")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # 1
    air<-na.omit(airquality)
    oz<-air$Ozone
    Temp<-air$Temp
    model1<-lm(oz~Temp)
    # 2
    model1pred<-reactive({
        TempInput<-input$sliderTemp
        predict(model1, newdata=data.frame(Temp=TempInput))
    })
    
    # 4
    output$plot1<-renderPlot({
        TempInput<-input$sliderTemp
        plot(airquality$Temp, airquality$Ozone, xlab = "Temperature", ylab = "Ozone", bty="n", pch=16)
        points(TempInput, model1pred(), col="red", pch=16, cex=2)
    })
    
    # 5
    output$pred1<-renderText({
        model1pred()
    })
    
})

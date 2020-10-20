
library(tidyverse)
library(ggplot2)
library(shiny)
strawberry=read.csv("C:/Users/Lenovo/Desktop/srawberry/strawberry.csv")

# Define UI for application that draws a histogram 
# Define UI for miles per gallon application
ui=shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("EDA of strawberry data "),
  
  sidebarPanel(
    selectInput("measurement",
                "measurement: ",
                c("",
                  unique(as.character(strawberry$measurement))))
  ),
  
  mainPanel(
    plotOutput("linePlot")
  )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$linePlot <- renderPlot({
    strawberry_measure=filter(strawberry,measurement == input$measurement)
    strawberry_measure=strawberry_measure %>% group_by(State, Year) %>% summarise(VALUE= mean(Value))
    
    ggplot(data = strawberry_measure, mapping = aes(x = Year, y = VALUE)) + geom_point(aes(color =
                                                                                     State)) + geom_line(aes(color = State)) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


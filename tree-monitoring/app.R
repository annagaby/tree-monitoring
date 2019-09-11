# Load relevant packages
library(tidyverse)
library(shinythemes)
library(leaflet)
library(sf)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # theme for the app
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Reforestation Project - Sacha Waysa Community"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year", 
                        "Select year:",
                        choices = c( "2019", "2020", "2021", "2022"),
                        selected = "2018"),
            tags$hr(style="border-color: gray;"),
            p("Select year of reforestation project. ")   
        ),

        # Show a plot of the generated distribution, p and map
        mainPanel(
           leafletOutput("mymap",height = 500),
           p
        )
    ),
    
    # Create footer
    tags$footer("Developed by Anna Calle <annagcalle@bren.ucsb.edu> in programming language R version 3.6.1 (2019-07-05). Code on", tags$a(href ="https://github.com/annagaby/tree-monitoring", target="_blank", "GitHub."))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # Generate map ouput
    output$mymap <- renderLeaflet({
        m
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

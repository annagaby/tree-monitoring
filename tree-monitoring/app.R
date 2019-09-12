# Load relevant packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(lubridate)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # theme for the app
    theme = shinytheme("flatly"),

    # Application title
    titlePanel(tags$img(src = "https://images.squarespace-cdn.com/content/5be1911ecc8fed6b42ebe87e/1546689900224-9JJK8UZ1EEEXGBI7NS3P/logo+yakum_for+dark+BG.png?format=1500w&content-type=image%2Fpng", height = 80, "   Reforestation Project - Sacha Waysa Community")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year", 
                        "Select year:",
                        choices = c( "2019", "2020", "2021", "2022"),
                        selected = "2019"),
            tags$hr(style="border-color: gray;"),
            p("Trees were planted in 2019 and monitored on a yearly basis until 2022. ")   
        ),

        # Show 4 tabs for: reforestation map, species composition, growth, and mortalities
        mainPanel(
            tabsetPanel(type = "tabs",
                              tabPanel("Map", leafletOutput("mymap",height = 500)),
                              tabPanel("Species", 
                                       br(),
                                       plotlyOutput("compositionPlot")),
                              tabPanel("Growth",
                                 plotOutput("fastestPlot"),
                                 plotOutput("slowestPlot")),
                              tabPanel("Mortalities",
                                       br(),
                                       plotOutput("mortalityPlot"))
            
            
           
           )
        )
    ),
    
    # Create footer
    tags$footer("Developed by Anna Calle <annagcalle@bren.ucsb.edu> in programming language R version 3.6.1 (2019-07-05). Code on", tags$a(href ="https://github.com/annagaby/tree-monitoring", target="_blank", "GitHub."))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Read files
    tree_data <- read_csv("tree_monitoring.csv")
    
    # Generate map ouput
    output$mymap <- renderLeaflet({
        
        # Filter by year and arrange data
        tree_data_arranged = tree_data %>% 
            filter( Year == input$year) %>% 
            arrange(-Height) # arrange so that dead plants are last and red color shows over green
        
        # Create tree location map
        pal <- colorFactor(c("green", "red"), tree_data_arranged$Alive_or_Dead) 
        
        leaflet(data = tree_data_arranged) %>%
            addTiles() %>% 
            addCircles(lng = ~Longitude,
                       lat = ~Latitude,
                       weight = 3, radius=2.5, 
                       color= ~pal(Alive_or_Dead),
                       stroke = TRUE, fillOpacity = 0.8,
                       popup = paste("<strong>Tree ID</strong>:", tree_data_arranged$ID, "<br>",
                                     "<strong>Species:</strong>", tree_data_arranged$Species, "<br>",
                                     "<strong>Height:</strong>", tree_data_arranged$Height, " m" ))
        
    })
    
    
    # Generate species composition output
    output$compositionPlot <- renderPlotly({
        
        # Filter tree data by year output
        tree_data_by_year <- tree_data %>% 
            filter(Year == input$year) %>% 
            group_by(Species) %>% 
            count(Species)
        
        # Create pie chart
        plot_ly(tree_data_by_year, labels = ~Species, values = ~n, type = 'pie', textposition = 'inside',
                textinfo = 'label+percent',
                hoverinfo = 'text',
                text = ~paste( n, ' trees'),
                showlegend = FALSE,
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1))) %>%
            layout(title = paste("Species Composition", input$year),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    
    # Generate fastest growth output
    output$fastestPlot <- renderPlot({
        
    })
    
    # Generate slowest growth output
    output$slowestPlot <- renderPlot({
        
    })
    
    
    # Generate mortalities output
    output$mortalityPlot <- renderPlot({
        
        # Filter mortalities by year
        mortalities_by_year <- tree_data %>% 
            select(Year, Cause_of_death) %>% 
            filter( Cause_of_death != "NA") %>%
            filter( Year == input$year) %>% 
            count(Cause_of_death, Year) %>%
            arrange(-n)
        
        # Create graph
        ggplot(mortalities_by_year, aes( x = Cause_of_death, y = n)) +
            geom_col() +
            xlab("Cause of Death") +
            ylab("Number of Dead Plants") +
            theme_classic() +
            ggtitle(paste("Causes of Plant Mortality", input$year))+
            theme(plot.title = element_text(hjust = 0.5))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

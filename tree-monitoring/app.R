# Load relevant packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(lubridate)
library(plotly)
library(RColorBrewer)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Theme for the app
    theme = shinytheme("flatly"),

    # Application title
    titlePanel(title = tags$img(src = "https://images.squarespace-cdn.com/content/5be1911ecc8fed6b42ebe87e/1546689900224-9JJK8UZ1EEEXGBI7NS3P/logo+yakum_for+dark+BG.png?format=1500w&content-type=image%2Fpng", height = 80, "   Reforestation Project - Sacha Waysa Community"),
               windowTitle = "Reforestation Project - Sacha Waysa Community"),
    
    # Navbar
    navbarPage("Tree Monitoring App",
               
               # First tab
               tabPanel(div(icon("info-circle"),"About"),
                        h1("The App"),
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                        h1("The Community: Sacha Waysa"),
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                        h1("Yakum"),
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
                          a(href="https://yakum.org", "Yakum's website.", target="_blank"))),
               
               # Second tab
               tabPanel(div(icon("map-pin"),"Map"),
                        tags$div(class="alert alert-dismissible alert-success",
                                 "Note: Click on individual trees to display info!"),
                        leafletOutput("mymap",height = 500),
                        # Fluid row for inputs
                        fluidRow(
                            column(6,
                                   selectInput("year_map",
                                               "Select year:",
                                               choices = c("2019","2020","2021","2022"))),
                            column(6,
                                   radioButtons("vis",
                                                "Color tree marker by:",
                                                choices = c("Survival", "Species"),
                                                selected = "Survival"))
                        )
                        ),
               
               # Third tab
               tabPanel(div(icon("chart-pie"),"Species"),
                        tags$div(class="alert alert-dismissible alert-success","Note: Hover on chart to display tree number!"),
                        plotlyOutput("compositionPlot"),
                        # Fluid row for inputs
                        fluidRow(
                            column(6,
                                   selectInput("year_pie",
                                               "Select year:",
                                               choices = c("2019","2020","2021","2022"),
                                               selected = "2019")))
                        ),
               
               # Fourth tab
               tabPanel(div(icon("chart-bar"),"Growth"),
                        fluidRow(
                            column(6,
                                   plotOutput("fastestPlot")),
                            column(6,
                                   plotOutput("slowestPlot"))
                        ),
                        
               # Fluid row for inputs
               fluidRow(
                   column(6,
                          selectInput("year_growth",
                                      "Select year:",
                                      choices = c("2019","2020","2021","2022"),
                                      selected = "2019")))),
               
               
               # Fifth tab
               tabPanel(div(icon("table"),"Mortalities"),
                        br(),
                        dataTableOutput("mortalityTable"),
                        # Fluid row for inputs
                        fluidRow(
                            column(6,
                                   checkboxGroupInput("mort", label = h3("Group mortalities by"), 
                                                      choices = list("Total" = 1,
                                                                     "Species" = 2,
                                                                     "Year" = 3),
                                                      selected = 1))))
    ),
            
    # Create footer
    br(),
    tags$footer("Developed by Anna Calle <annagcalle@bren.ucsb.edu> in programming language R version 3.6.1 (2019-07-05). Code on", tags$a(href ="https://github.com/annagaby/tree-monitoring", target="_blank", icon("github"),"GitHub.")),
    br()
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Read files
    tree_data <- read_csv("tree_mock_data.csv")
    sach_polygon <- st_read(dsn = ".", layer = "ref_area")
    
    # Change Height NA's to zeros
    tree_data$Height[is.na(tree_data$Height)] <- 0
    
    # Generate map ouput
    output$mymap <- renderLeaflet({
        
        
        # Filter by year and arrange data
        tree_data_arranged <- tree_data %>% 
            filter(Year == input$year_map) %>% 
            arrange(-Height)  # arrange so that dead plants are last and red color shows over green
            
        
        # Create tree location map
        
        titles <-  if(input$vis == "Survival"){ paste("Tree Survival", input$year_map)
        } else {
            paste("Species Composition", input$year_map)
        }
        
        # Palette for legend
        pal <- if (input$vis == "Survival") {colorFactor(c("#7EAB46", "#CE3E35"), tree_data_arranged$Alive_or_Dead)
        } else { colorFactor(c("#EA9631", "#CE3E35","#F5F9F2", "#7EAB46", "#3AA1D9","#C65EAA"), tree_data_arranged$Species) }
        
        # values
        values <- if (input$vis == "Species") {~Species
             } else { ~Alive_or_Dead}
        
        # Icons with different colors depending on vis input selected
        
        icons <- if (input$vis == "Species") {
            # Icons for species input
            awesomeIcons(
                icon = 'fa-tree',
                iconColor = 'black',
                library = 'fa',
                markerColor = ifelse( tree_data_arranged$Species == "Caoba", "orange",
                                      ifelse(tree_data_arranged$Species == "Chonta", "red",
                                             ifelse(tree_data_arranged$Species == "Guaba", "white",
                                                    ifelse(tree_data_arranged$Species == "Jackfruit", "green",
                                                           ifelse(tree_data_arranged$Species == "Morete", "blue", "purple")))))
            )} else {
            # Icons for survival input
        awesomeIcons(
                icon = 'fa-tree',
                iconColor = 'black',
                library = 'fa',
                markerColor = ifelse( tree_data_arranged$Alive_or_Dead == "Dead", "red", "green")
            )}
        
        # Map
        map <- leaflet(data = tree_data_arranged) %>%
            addTiles() %>% 
            addPolygons(data = sach_polygon,
                        color = "forestgreen",
                        weight = 1,
                        fillColor = "forestgreen") %>% 
            addAwesomeMarkers(data = tree_data_arranged,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       icon = icons,
                       popup = paste("<strong>Tree ID</strong>:", tree_data_arranged$ID, "<br>",
                                     "<strong>Species:</strong>", tree_data_arranged$Species, "<br>",
                                     "<strong>Height:</strong>", tree_data_arranged$Height, " m" )) %>%
            addLegend(pal = pal, values = values, opacity = 1, title = titles) %>% 
            addMiniMap(zoomLevelOffset = -8) %>% 
            addScaleBar(position = "bottomleft")
        
    })
    
    # Generate species composition output
    output$compositionPlot <- renderPlotly({
        
        # Count trees by species
        tree_data_count <- tree_data %>% 
            filter(Year == input$year_pie) %>%  
            group_by(Species) %>% 
            count(Species)
        
        # Create pie chart
        plot_ly(tree_data_count,
                labels = ~Species,
                values = ~n, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = 'black'),
                hoverinfo = 'text',
                text = ~paste( n, ' trees'),
                showlegend = FALSE,
                marker = list(colors = brewer.pal(n = 6, name = "Set2"),
                              line = list(color = '#FFFFFF',
                                          width = 1))) %>%
            layout(title = paste("Species Composition", input$year_pie),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    
    # Generate fastest growth output
    output$fastestPlot <- renderPlot({
        
        tree_data$Height[is.na(tree_data$Height)] <- 0
        
        
        top5_fastest <- tree_data %>% 
            filter( Year == input$year_growth) %>%
            group_by(Species) %>% 
            summarize(av_height = mean(Height)) %>% 
            arrange( -av_height) %>% 
            head(5) 
        
        
        top5_fastest$Species <- factor(top5_fastest$Species, levels = top5_fastest$Species)
        
        ggplot(top5_fastest, aes(x = Species, y = av_height)) + 
            geom_bar(width = 0.75, fill="gold2", stat = "identity") +
            theme_classic() +
            scale_y_continuous(expand = c(0,0)) +
            xlab("Species") +
            ylab("Average Height (m)") +
            ggtitle(paste("Top 5 Fastest Growing Species", input$year_growth)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position="none")
    })
    
    # Generate slowest growth output
    output$slowestPlot <- renderPlot({
        
        tree_data$Height[is.na(tree_data$Height)] <- 0
        
        
        top5_slowest <- tree_data %>% 
            filter( Year == input$year_growth) %>%
            group_by(Species) %>% 
            summarize(av_height = mean(Height)) %>% 
            arrange( av_height) %>% 
            head(5) 
        
        
        top5_slowest$Species <- factor(top5_slowest$Species, levels = top5_slowest$Species)
        
        ggplot(top5_slowest, aes(x = Species, y = av_height)) + 
            geom_bar(width = 0.75, fill="darkturquoise", stat = "identity") +
            theme_classic() +
            scale_y_continuous(expand = c(0,0)) +
            xlab("Species") +
            ylab("Average Height (m)") +
            ggtitle(paste("Top 5 Slowest Growing Species", input$year_growth)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position="none")
    })
    
    
    # Generate mortalities output
    output$mortalityTable <- renderDataTable({
        
        table <- if (all(input$mort == c("1", "2","3"))){
            tree_data %>%
                count(Alive_or_Dead, Species, Year)
        } else if (all(input$mort == c("2","3"))){
            tree_data %>%
                count(Alive_or_Dead, Species, Year)
        } else if (all(input$mort == c("1","3"))){
            tree_data %>%
                count(Alive_or_Dead, Year)
        } else if (all(input$mort == c("1","2"))){
            tree_data %>%
                count(Alive_or_Dead, Species)
        } else if (input$mort == c("1")){
           tree_data %>%
               count(Alive_or_Dead)
       } else if (input$mort == c("2")){
           tree_data %>%
               count(Alive_or_Dead, Species)
       } else if (input$mort == c("3")){
           tree_data %>%
               count(Alive_or_Dead, Year)
       } 
       
        final_table <- table %>% 
            spread(Alive_or_Dead, n) %>% 
            mutate("Mortality Rate" = round(Dead/(Alive + Dead)*100, digits = 2))
        
        # Change NA's to zeros
        final_table[is.na(final_table)] <- 0
        
        final_table
    }, options= list(paging = FALSE, searching = FALSE,  pageLength = 6))
}

# Run the application 
shinyApp(ui = ui, server = server)

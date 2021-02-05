#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Attach packages
library(tidyverse)
library(shiny)
library(bslib)
library(here)

read_csv(here("lionfish_shiny_app", "lionfish_data.csv"))

ui <- fluidPage(theme = "our_theme.css",
                
                navbarPage("THIS IS MY TITLE!",
                           tabPanel("Tab 1",
                                    sidebarLayout(
                                        sidebarPanel("WIDGETS!",
                                                     checkboxGroupInput(inputId = "pick_species",
                                                                        label = "Choose species:",
                                                                        choices = unique(starwars$species))
                                        ),
                                        mainPanel("OUTPUT!",
                                                  plotOutput("sw_plot"))
                                    )
                           ),
                           tabPanel("Tab 2"),
                           tabPanel("Tab 3")
                           
                )
                
)

server <- function(input, output) {
    
    sw_reactive <- reactive({
        
        starwars %>%
            filter(species %in% input$pick_species)
        
    })
    
    output$sw_plot <- renderPlot(
        ggplot(data = sw_reactive(), aes(x = mass, y = height)) +
            geom_point(aes(color = species))
    )
    
}

shinyApp(ui = ui, server = server)

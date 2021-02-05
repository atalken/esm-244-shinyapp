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

lionfish <- read_csv(here("lionfish_shiny_app", "lionfish_data.csv"))

ui <- fluidPage(theme = "our_theme.css",
                
                navbarPage("THIS IS MY TITLE!",
                           ####### Start Tab Panel 
                           tabPanel("Select Box - fish diet -> output size",
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
                           tabPanel("Slider Bar - fish depth -> fish weight",
                                    sidebarLayout(
                                        sidebarPanel("Age Widget",
                                                     sliderInput(inputId = "select_age",
                                                                 label = "Select Age Range",
                                                                 min = 0, max = 900,
                                                                 value = c(40, 60))),
                                        mainPanel("Output", textOutput("age_character"))
                                    )),
                           tabPanel("Radio buttons - selecting site _> output spatial of lionfish occurence",
                                    sidebarLayout(
                                        sidebarPanel("select site",
                                                     radioButtons("radio", label = h3("Radio buttons"),
                                                                  choices = list("Choice 1" = 1,
                                                                                 "Choice 2" = 2,
                                                                                 "Choice 3" = 3), 
                                                                  selected = 1)),
                                        mainPanel("output", textOutput("output!"))
                                        
                                    )),
                           tabPanel("Select Box - select prey -> output scatterplot fish weight vs item (prey) weight",
                                    sidebarLayout(
                                        sidebarPanel("select prey",
                                                     selectInput("select", label = h3("Select box"), 
                                                                 choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                                 selected = 1)),
                                        mainPanel("output", textOutput("output!"))
                                    )
                               
                           )
                           
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

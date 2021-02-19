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
library(janitor)


my_bs_theme <- bs_theme(
    bg = "darkseagreen",
    fg = "honeydew",
    primary = "white",
    base_font = font_google("Questrial")
)

lionfish <- read_csv(here("lionfish_shiny_app", "lionfish_data.csv")) %>% 
    clean_names() %>% 
    mutate(common_name = case_when(
        item_genus_species == "unid" ~ "Unknown", # CHECK THIS???
        item_genus_species == "Pterois_volaitans" ~ "Red Lionfish",
        item_genus_species == "Lysmata_sp" ~ "Peppermint Shrimp",
        item_genus_species == "Mysidium_spp" ~ "Mysid Shrimp", #check
        item_genus_species == "Mycteroperca_venenosa" ~ "Yellofin grouper",
        item_genus_species == "Gobiosoma_prochilos" ~ "Gobiosoma prochilos", #couldnt find it
        item_genus_species == "Gonodactylus_smithii" ~ "Purple Spot Manits Shrimp",
        item_genus_species == "Canthigaster_rostrata"~ "Sharpnose Puffer",
        item_genus_species == "empty" ~ "Empty",
        item_genus_species == "Cryptotomus_roseus" ~ "Bluelip Parrotfish",
        item_genus_species == "Ophioblennius_atlanticus" ~ "Redlip Blenny",
        item_genus_species == "Chromis_multilineata" ~ "Brown Chromis",
        item_genus_species == "Stegastes_partitus" ~ "Bicolor Damselfish",
        item_genus_species == "Gramma_loreto" ~ "Royal Gramma",
        item_genus_species == "Callinectes_sapidus" ~ "Chesapeake Blue Crab",
        item_genus_species == "Malacoctenus_trinagulatus" ~ "Saddle Blenny",
        item_genus_species == "Apogon_spp." ~ "Cardinalfish",
        item_genus_species == "Epinephelus_spp." ~ "Predatory Ray-Finned Fish", # better name???
        item_genus_species == "Thalassoma_bifasciatum" ~ "Blueheaded Wrasse",
        item_genus_species == "Liopropoma_carmabi" ~ "Candy Basslet"
    ))

ui <- fluidPage(theme = my_bs_theme,
                
                navbarPage("THIS IS MY TITLE!",
                 
                           
                           ### Home Page###
                           tabPanel("Home Page"),
                           
                           
                           ########### TAB 1 species info ######
                           
                           tabPanel("Select Box - select species -> output information about species",
                                    sidebarLayout(
                                      sidebarPanel("select prey",
                                                   selectInput("select", label = h3("Select box"), 
                                                               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                               selected = 1)),
                                      mainPanel("output", textOutput("output!"))
                                    )
                                    
                           ),
                           
                           
                        
                           #######  Tab 2  ####
                           tabPanel("Select Box - fish diet -> output size",
                                    sidebarLayout(
                                        sidebarPanel("WIDGETS!",
                                                     checkboxGroupInput(inputId = "pick_species",
                                                                        label = "Choose prey species:",
                                                                        choices = 
                                                                          unique(lionfish$common_name))
                                        ),
                                        mainPanel("Graph of lion fish length v weight RE diet!",
                                                  plotOutput("diet_plot"))
                                        )
                                    ),
                          
                          ####### Tab 3
                           tabPanel("Slider Bar - fish depth -> fish weight",
                                    sidebarLayout(
                                        sidebarPanel("Depth Widget",
                                                     sliderInput(inputId = "select_depth",
                                                                 label = "Select Depth Range",
                                                                 min = 0, max = 40,
                                                                 value = c(5, 10))
                                         ),
                                        mainPanel("Depth Output", plotOutput("depth_plot"))
                                        )
                                    ),
                           
                          ######## Tab 4
                           tabPanel("Radio buttons - selecting site -> output spatial of lionfish occurence",
                                    sidebarLayout(
                                        sidebarPanel("select site",
                                                     radioButtons("radio", label = h3("Radio buttons"),
                                                                  choices = unique(lionfish$location), 
                                                                  selected = "Paraiso")),
                                  
                                        mainPanel("output", plotOutput("tmap!"))
                                        
                                    ))

                           
                )
                
)
############### End USER INTERFACE : START SERVER #############

server <- function(input, output) {
  
  ### Tab 1 Reactive output 
  
  
   ###### Tab 2 Reactive output #############
    diet_reactive <- reactive({
        
        lionfish %>%
            filter(common_name %in% input$pick_species)
        
    })
    
    output$diet_plot <- renderPlot(
        ggplot(data = diet_reactive(), aes(x = total_length_cm, y = total_weigth_gr)) +
            geom_point(aes(color = common_name)) +
          theme_minimal() +
          labs(title = "Title",
               x = "Length (cm)",
               y = "Weight (g)")
    )
  ### Tab 3 Reactive outputt#### 
    
    
    depth_reactive <- reactive({
      
      lionfish %>%
        filter(depth_m >= input$select_depth[1]) %>% 
        filter(depth_m <= input$select_depth[2])
      
    })
    
    output$depth_plot <- renderPlot(
      ggplot(data = depth_reactive(), aes(x = depth_m, y = total_weigth_gr)) +
        geom_point()
    )
    ######## Tab 4 reactive output###########
}

shinyApp(ui = ui, server = server)

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
library(tmap)

# Create the theme
my_bs_theme <- bs_theme(
    bg = "darkseagreen",
    fg = "honeydew",
    primary = "white",
    base_font = font_google("Lato"),
    heading_font = font_google("Merriweather")
)

# Read in the data
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
                
                navbarPage("Lionfish in the Mexican Carribean",
                 
                           
                           ### Home Page###
                           tabPanel("Home Page", mainPanel("Welcome! Are you interested in learning about invasive lionfish in Mexican Carribean waters? This Shiny App allows you to explore data on lionfish and their prey species that was collected by Juan Carlos Villasenor along the central Mexican Carribean coast in 2010. In this app, you will be able to explore the following:
                                                           * Descriptions and photos of the observed lionfish prey species
                                                           * The association between lionfish prey and the size of the lionfish
                                                           * The relationship between observed depth of lionfish and their weight
                                                           * An interactive spatial map depicting lionfish occurences based on the sampling site.
                                                           Data Citation: 
                                                           
                                                           Shiny App created by Grace Kumaishi, Anastasia Kunz and Anna Talken
                                                           ", textOutput("output"))
                                    ),
                           
                           
                           ########### TAB 1 species info ######
                           
                           tabPanel("Prey Descriptions",
                                    sidebarLayout(
                                      sidebarPanel("select prey",
                                                   selectInput("select", label = h3("Select box"), 
                                                               choices = unique(lionfish$common_name))),
                                      mainPanel("output", textOutput("output!"))
                                    )
                                    
                           ),
                           
                           
                        
                           #######  Tab 2  ####
                           tabPanel("Body Size to Prey Choice",
                                    sidebarLayout(
                                        sidebarPanel("WIDGETS!",
                                                     checkboxGroupInput(inputId = "pick_species",
                                                                        label = "Choose prey species:",
                                                                        choices = 
                                                                          unique(lionfish$common_name))
                                        ),
                                        mainPanel("A comparison of lionfish size (in length and weight) to their selected prey species",
                                                  plotOutput("diet_plot"))
                                        )
                                    ),
                          
                          ####### Tab 3
                           tabPanel("Fish Weight at Varying Depths",
                                    sidebarLayout(
                                        sidebarPanel(
                                                     sliderInput(inputId = "select_depth",
                                                                 label = "Select Depth Range",
                                                                 min = 0, max = 40,
                                                                 value = c(5, 10))
                                         ),
                                        mainPanel("Depth Output", plotOutput("depth_plot"))
                                        )
                                    ),
                           
                          ######## Tab 4
                           tabPanel("Interactive Spatial Map",
                                    sidebarLayout(
                                        sidebarPanel("select site",
                                                     radioButtons(inputId = "select_location", label = h3("Select Location"),
                                                                  choices = unique(lionfish$location), 
                                                                  selected = "Paraiso")),
                                  
                                        mainPanel("Observations of Lionfish by location", plotOutput("location_plot"))
                                        
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
          labs(x = "Length (cm)",
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
        geom_point() +
        theme_minimal()+
        labs(x = "Depth (m)",
             y = "Lionfish weight (g)")
    )
    ######## Tab 4 reactive output###########

   spatial_reactive <- reactive({
    
      lionfish %>% 
       select(location, latitude, longitude, common_name, date_dd_mm_yyyy) %>% 
       filter(location %in% input$select_location)
     
     output$location_plot<- renderPlot(
       ggplot() +
         geom_point(data = spatial_reactive(), aes(x = lat, y = long)) 
     ) # NEED TO MAKE IT SF, ALSO NEED TO DO SOMETHING DIFF FOR TMAP ILL LOOK DEEPER
   }) 
    
    
    }

shinyApp(ui = ui, server = server)

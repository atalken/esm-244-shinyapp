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
library(sf)
library(sp)

tmap_mode("view")

# Create the theme
my_bs_theme <- bs_theme(
    bg = "#4e5d6c",
    fg = "honeydew",
    primary = "white",
    base_font = font_google("Lato"),
    heading_font = font_google("Lato")
)

# Read in the data
lionfish <- read_csv(here("lionfish_shiny_app", "lionfish_data.csv")) %>% 
    clean_names() %>% 
    mutate(common_name = case_when(
        item_genus_species == "unid" ~ "Unknown", 
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
    )) %>% 
  mutate(prey_info = case_when(
    common_name == "Peppermint Shrimp" ~ "Peppermint shrimp are native to the Gulf of Mexico and are small shrimp that are aptly named for their red striped bodies. They are detritus feeders and typically eat decomposing material in the ocean.",
    common_name == "Mysid Shrimp" ~ "These small crustaceans are found in a variety of ecosystems but primarily in the Gulf of Mexico. Their two antennae and big eyes make them easy to recognize, and they feed mainly on algae and other detritus.",
    common_name == "Cardinalfish" ~ "Cardinalfish are in the family ‘Apogonidae’ and are typically found in shallow tropical reefs. They often find refuge inside conch shells,and typically feed at night on benthic crustaceans and other small invertebrates. These small fish have two dorsal fins and large eyes and mouths that makes them recognizable.",
    common_name == "Saddle Blenny" ~ "The Saddle Blenny is typically found among shallow waters in rocky and coral reefs throughout the Caribbean.  Colors vary, but many are red and brown with distinct stripes. These fish feed on small organisms such as benthic worms, shrimp and crabs.")) %>% 
  drop_na(item_total_length_cm) %>% 
  mutate(item_total_length_cm = as.numeric(item_total_length_cm))

# Read in the data for spatial use
lionfish_spatial <- st_read(here("lionfish_shiny_app", "lionfish_data.csv")) %>% 
  clean_names() %>% 
  mutate(common_name = case_when(
    item_genus_species == "unid" ~ "Unknown", 
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
  )) %>% 
  mutate(prey_info = case_when(
    common_name == "Peppermint Shrimp" ~ "Peppermint shrimp are native to the Gulf of Mexico and are small shrimp that are aptly named for their red striped bodies. They are detritus feeders and typically eat decomposing material in the ocean.",
    common_name == "Mysid Shrimp" ~ "These small crustaceans are found in a variety of ecosystems but primarily in the Gulf of Mexico. Their two antennae and big eyes make them easy to recognize, and they feed mainly on algae and other detritus.",
    common_name == "Cardinalfish" ~ "Cardinalfish are in the family ‘Apogonidae’ and are typically found in shallow tropical reefs. They often find refuge inside conch shells,and typically feed at night on benthic crustaceans and other small invertebrates. These small fish have two dorsal fins and large eyes and mouths that makes them recognizable.",
    common_name == "Saddle Blenny" ~ "The Saddle Blenny is typically found among shallow waters in rocky and coral reefs throughout the Caribbean.  Colors vary, but many are red and brown with distinct stripes. These fish feed on small organisms such as benthic worms, shrimp and crabs."))

ui <- fluidPage(theme = my_bs_theme, 
                
                
                navbarPage("Lionfish in the Mexican Carribean",
                 
                           ##### Home Page ######
                           
                           tabPanel("Home Page", mainPanel(width = 11, column(11, offset = 1,
                                                          h3("Welcome!", align = "center"), 
                                                          h5("Are you interested in learning about invasive lionfish in Mexican Carribean waters? This Shiny App allows you to explore data on lionfish and their prey species that was collected by Bren PhD student Juan Carlos Villasenor along the central Mexican Carribean coast in 2010."),
                                                          br(),
                                                          HTML('<center><img src="lionfish.jpg" width="600"></center>'),
                                                          p("Photo by: Michael Aston", align = "center"),
                                                          br(),
                                                          h5("In this app, you will be able to explore the following:"),
                                                          p("1) Descriptions and photos of the observed lionfish prey species"),
                                                          p("2) The association between lionfish prey and the size of the lionfish"),
                                                          p("3) The relationship between observed depth of lionfish and their weight"),
                                                          p("4) An interactive spatial map depicting lionfish occurrences based on the sampling site"),
                                                          br(),
                                                          h5("Data Citation:"),
                                                          p("Villaseñor-Derbez, JC. (2010). Lionfish Biometry, https://github.com/jcvdav/lionfish_biometry/tree/master/data"),
                                                          br(),
                                                          h6(em("Shiny App created by Grace Kumaishi, Anastasia Kunz and Anna Talken"), align = "center"),
                                                          textOutput("output"))
                                    )),
                           
                           ##### Tab 1 #####
                           
                           tabPanel("Prey Descriptions",
                                    sidebarLayout(
                                      sidebarPanel(selectInput(inputId = "select_prey", 
                                                               label = h5("Select prey species:"), 
                                                               choices = list("Peppermint Shrimp" = "pep",
                                                                              "Mysid Shrimp" = "mys",
                                                                              "Cardinalfish" = "car",
                                                                              "Saddle Blenny" = "sad"),
                                                               selected = "pep"),
                                                               ),
                                      mainPanel("Top Four Observed Prey Species",
                                                uiOutput("img1"),
                                                textOutput("description"))
                                    )
                           ),
                           
                           #####  Tab 2  #####
                           
                           tabPanel("Size Analysis",
                                    sidebarLayout(
                                        sidebarPanel(checkboxGroupInput(inputId = "pick_species",
                                                                        label = h5("Choose prey species:"),
                                                                        choices = unique(lionfish$common_name))
                                        ),
                                        mainPanel("Lionfish Size versus Prey Size",
                                                  plotOutput("diet_plot"),
                                                  "This graph explores the relationship between lionfish size and prey size. Invasive lionfish eat a wide variety of marine organisms and generally consume whatever comes across their path! Juan Carlos wanted to investigate the relationship between a lionfish’s size and what prey species made up their diet. To do this, he caught lionfish and identified the prey species in their stomach. His findings are displayed in this exploratory plot.")
                                        )
                                    ),
                          
                           ##### Tab 3 #####
                          
                           tabPanel("Depth Analysis",
                                    sidebarLayout(
                                        sidebarPanel(sliderInput(inputId = "select_depth",
                                                                 label = h5("Select depth range:"),
                                                                 min = 0, max = 40,
                                                                 value = c(5, 10))
                                         ),
                                        mainPanel("Lionfish Depth Analysis", plotOutput("depth_plot"), 
                                                  "When speaking with Juan Carlos about the data, we learned that not much is known about the vertical distribution of lionfish within a water column. We were curious to see if larger lionfish of reproductive age are able to reside at deeper depths, perhaps making the species more difficult to cull. This exploratory plot shows the depth at which individual lion fish were caught plotted against individual fish weight in grams. ")
                                        )
                                    ),
                           
                           ##### Tab 4 #####
                          
                           tabPanel("Spatial Analysis",
                                    sidebarLayout(
                                       sidebarPanel(radioButtons(inputId = "select_site", 
                                                                  label = h5("Select site:"),
                                                                  choices = unique(lionfish_spatial$location), 
                                                                  selected = "Paraiso")),
                                  
                                        mainPanel("Observations of Lionfish by Location", tmapOutput("location_plot"), "This interactive spatial map can be used to explore the various locations where Juan Carlos collected lionfish data from. All 10 of these sites are located on the Caribbean Sea side of Mexico, near the town of Puerto Aventuras. Choose a location and click on the dot on the map to see how many fish observations were recorded at that site!")
                                        
                                    ))
                )
)

#################### End USER INTERFACE : START SERVER ####################

server <- function(input, output) {
  
  ##### Tab 1 Reactive output #####
  
    output$img1 <- renderUI({
      if(input$select_prey == "pep"){
        img(height = "80%", width = "80%", src = 'peppermint.jpg')}
      else if(input$select_prey == "mys"){
        img(height = "80%", width = "80%", src = 'mysid.jpg')}
      else if(input$select_prey == "car"){
        img(height = "80%", width = "80%", src = 'cardinalfish.jpg')}
      else if(input$select_prey == "sad"){
        img(height = "80%", width = "80%", src = 'blenny.jpg')}
      }
    )
    
    output$description <- renderText({
      if( input$select_prey=="pep") {
        ("Peppermint shrimp are native to the Gulf of Mexico. These small shrimp are aptly named for their red striped bodies. They are detritus feeders and typically eat anemones and other decomposing material. (Photo by: Jasper Nance)")
      }
      else if (input$select_prey=="mys") {
        ("These small crustaceans are found in a variety of marine ecosystems, but primarily in the Gulf of Mexico. Their two antennae and big eyes make them easy to recognize, and they can be found feeding mainly on algae and other detritus. (Photo by: Dean Janiak)")
      }
      else if (input$select_prey=="car") {
        ("Cardinalfish are in the family ‘Apogonidae’ and are typically found in shallow tropical reefs. They often find refuge inside conch shells, and typically feed at night on benthic crustaceans and other small invertebrates. These small fish have two dorsal fins and large eyes and mouths that makes them recognizable. (Photo by: Francois Libert)")
      }
      else if(input$select_prey=="sad") {
        ("The Saddle Blenny is typically found among shallow waters in rocky and coral reefs throughout the Caribbean.  Colors vary, but many are red and brown with distinct stripes. These fish feed on small organisms such as benthic worms, shrimp and crabs. (Photo by: Flickr user @kryn13)")
      }})
  
  ##### Tab 2 Reactive output #####
    
    diet_reactive <- reactive({
        
        lionfish %>%
            filter(common_name %in% input$pick_species)
        
    })
    
    output$diet_plot <- renderPlot(
        ggplot(data = diet_reactive(), aes(x = total_length_cm, y = item_total_length_cm)) +
            geom_point(aes(color = common_name)) +
          theme_minimal() +
          labs(x = "Lionfish length (cm)",
               y = "Prey length (cm)",
               color = "Prey Species")
    )
  
  ##### Tab 3 Reactive output ##### 
    
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

  ##### Tab 4 reactive output #####

   spatial_reactive <- reactive({
    
      lionfish_spatial %>% 
       select(latitude, longitude, location) %>% 
       count(location, latitude, longitude) %>% 
       st_as_sf(., coords=c("longitude", "latitude"), crs = 4483) %>% 
       rename(Count = n,
              Location = location) %>% 
       filter(Location %in% input$select_site)
   })
     
     output$location_plot<- renderTmap({
       tm_shape(spatial_reactive())+
         tm_dots()+
         tm_basemap("OpenStreetMap")
     }) 
    
    
  
    }

shinyApp(ui = ui, server = server)

# THIS APPLICATION HELPS FIND THE NEAREST HOSPITAL IN VICTORIA, AUSTRALIA BASED ON LGA (LOCAL GOVERNMENT AREA), TYPE OF HOSPITAL (PUBLIC OR PRIVATE)
# AND THE PREFERRED LANGUAGE OF THE USER (ENGLISH, HINDI, MANDARIN, ARABIC ETC.)

# THIS APPLICATION IS DEVELOPED BY HYGIEIA, A PLATFORM FOR PREVENTING INFECTIONS AT THE WORKPLACE 

# Initializing all packages

library(shiny)
library(leaflet)
library(shinyalert)
library(shinydashboard)
library(sp)
library(leaflet)
library(readxl)

Hospitals <- read_excel("Hospital_Locations.xlsx") # Reading in the Hospitals Data File

langVector <- c("English", "Kannada", "Hindi", "Malayalam", "Bengali", "Persian", "Bosnian", "Croatian", "Serbian", "Cantonese",
                "Chinese", "Punjabi", "Urdu", "Greek", "Mandarin", "French", "Turkish", "Farsi", "Tamil", "Italian", "Macedonian",
                "Spanish", "Marathi", "Sinhalese", "Arabic", "Russian", "Vietnamese", "Polish", "Romanian", "Malay", "Gujarati",
                "Czech", "Slovak", "Telugu", "Zulu", "Ukranian")

langProb <- c(0.65, rep(0.1, 35)) # Creating the language probabilities, English being the highest

set.seed(30046572) # Setting seed, using my student id for it

Hospitals$Languages <- sample(langVector, size = nrow(Hospitals), replace = TRUE, prob = langProb) # Randomly sampling the languages, also ading a new column to the dataset

Hospitals$Languages[Hospitals$LGAName == "MELBOURNE" & Hospitals$Type == "PRIVATE"] <- "English" # Changing the Language to English to show clustering effect at launching

# UI side processing design

ui <- dashboardPage(skin = "blue", # Giving a blue header
                    
                    dashboardHeader(title = "Hospital Finder" # Dashboard title
                                    
                    ),
                    
                    dashboardSidebar( # Initializing the dashboard sidebar
                      
                      fluidRow(column(12, div(style = "height:100px", # Creating the LGA filter
                                              
                                              selectInput("SUBURB", h5(strong(em("LOCAL GOVERNMENT AREA"))), 
                                                          
                                                          choices = c(unique(as.character(Hospitals$LGAName))), # Giving all the options for LGA's
                                                          
                                                          selected = "MELBOURNE", multiple = FALSE)) # Declaring multi-select as FALSE
                    )),
                      
                      fluidRow(column(12, div(style = "height:100px", # Creating Hospital Type filter
                                              
                                              selectInput("TYPE", h5(strong(em("HOSPITAL TYPE"))),
                                                          
                                                          choices = c(unique(as.character(Hospitals$Type))), # Giving all the options for Hospital Type
                                                          
                                                          selected = "PRIVATE", multiple = FALSE)) # Declaring multi-select as FALSE
                    )),
                      
                      fluidRow(column(12, div(style = "height:100px", # Creating Hospital Type filter
                                              
                                              selectInput("LANGUAGE", h5(strong(em("LANGUAGE PREFERENCE"))),
                                                          
                                                          choices = c(unique(as.character(Hospitals$Languages))), # Giving all the options for Hospital Type
                                                          
                                                          selected = "English", multiple = FALSE)) # Declaring multi-select as FALSE
                    ))
                      
                    ), # Initializing a sidebar
                    
                    dashboardBody(useShinyalert(), # Shiny alert for cool popups
                                  
                                  tags$head(tags$style(HTML('                       
                                                            .main-header .logo {
                                                            font-family: "Georgia", Times,
                                                            "Times New Roman",
                                                            font-weight: bold;
                                                            font-size: 24px;
                                                            font-style: italic;
                                                            }
                                                            '))), # A CSS template for the font size and font
                                  
                                  fluidRow(
                                    
                                    tabBox(height = "1200px", width = "1000px",
                                           
                                           tabPanel(title = tagList(icon("project-diagram", class = "fas fa-project-diagram") # Creating the tabset panels
                                                                    
                                                                    , "SPREAD OF HOSPITALS ACROSS VICTORIA"),
                                                    
                                                    box(leafletOutput("HospitalMap"), status = "primary", solidHeader = TRUE, # Hospital distribution map of Vitoria
                                                        
                                                        title = "HOSPITALS IN VICTORIA (MAP)", width = 12, height = 512, collapsible = TRUE, align = "center"), # Leaflet ouput box
                                                    
                                                    box(DT::dataTableOutput("DataTable"), status = "primary", solidHeader = TRUE, # Hospital distribution data of Vitoria
                                                        
                                                        title = "HOSPITALS IN VICTORIA (DATA)", width = 12, height = 612, collapsible = TRUE) # Data table ouput box
                                                    )
                                           
                                            )
                                          )
                      ))

# Server side processing design

server <- function(input, output, session) { # Starting the server side application
  
  shinyalert(title = "Nearest Hospital Finder!", type = "info", text = "Select the Suburb in which you want to find Hospitals, Type of Hospital and Preferred Language you want
             using the dropdown menu on the left and we will find you the best Hospitals!") # Shinyalert to let the user know what the widget is about
  
  
  output$HospitalMap <- renderLeaflet({ # Declaring the leaflet processing function using shiny's renderLeaflet
    
        LGA_Filtered <- input$SUBURB # Fetching user input from the LGA filter
    
        Type_Filtered <- input$TYPE # Fetching user input from the Hospital Type filter
    
        Language_Filtered <- input$LANGUAGE
    
        if(LGA_Filtered != "All" & Type_Filtered != "All" & Language_Filtered != "All") { # Establishing an if-else condition based on user input, to restrict the data if "All" is not selected in the filter
      
          Hospitals_Filtered <- subset(Hospitals, LGAName == LGA_Filtered & Type == Type_Filtered & Languages == Language_Filtered) # Filtering Hospital data based on some condition
      
          }
    
        Hospitals_Filtered <- as.data.frame(Hospitals_Filtered) # Converting a matrix into a dataframe
    
    
        coordinates(Hospitals_Filtered) <- ~Longitude+Latitude # Converting character coordinated into spatial coordinates
    
    
        leaflet(Hospitals_Filtered) %>% addTiles() %>% # Calling the leaflet function on our filtered data and calling addTiles on top of it
                                    addMarkers(clusterOptions = markerClusterOptions(), # Adding markers on the map and clustering them
                                    label = toupper(Hospitals_Filtered$Hospital)) # Giving the markers a label and an icon
                                    
  })
  
  output$DataTable <- DT::renderDataTable({ # Rendering a data table
    
        LGA_Filtered <- input$SUBURB # Fetching the user's input for LGA
    
        Type_Filtered <- input$TYPE # Fetching the user's input for Hospital Type
    
        Language_Filtered <- input$LANGUAGE
    
        if(LGA_Filtered != "All" & Type_Filtered != "All" & Language_Filtered != "All") { # Filtering Hospital data based on some condition
      
          Hospitals_Filtered <- subset(Hospitals, LGAName == LGA_Filtered & Type == Type_Filtered & Languages == Language_Filtered) # Subsetting data based on condition
      
          if(nrow(Hospitals_Filtered) == 0) { # Checking the condition of exhausting data
        
          shinyalert(title = "Not enough data!", type = "error", text = "Sorry, there are no Hospitals with the selected filters!") # Displaying an error for running out of data
        
          }
      
        }
    
        Hospitals_Filtered <- as.data.frame(Hospitals_Filtered) # Converting the filtered dataset into a dataframe
    
        Hospitals_Filtered[, c(3:7)] # Returning the filetred dataset
    
  })
  
} # Closing the serve side

shinyApp(ui = ui, server = server) # Calling the application to launch
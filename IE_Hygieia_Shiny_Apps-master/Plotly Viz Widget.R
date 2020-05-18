# THIS APPLICATION HELPS THE USER UNDERSTAND THE DISTRIBUTION OF CERTAIN TOP DISEASES ACROSS INDUSTRIES, HOW THEY SPREAD AND WHAT
# HAS BEEN THEIR TREND OF SPREAD OVER THE PAST FEW YEARS AS WELL AS HOW GENDER AND AGE PLAYS AN IMPORTANT ROLE IN SPREAD

# THIS APPLICATION HAS BEEN DEVELOPED BY HYGIEIA, A PLATFORM FOR PREVENTING INFECTIONS AT A WORKPLACE

# Initializing all packages

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(shinyalert)
library(viridis)
library(RColorBrewer)
library(viridisLite)

VicData <- read_excel("Victoria Infections Data.xlsx", col_types = c("text", "text", "numeric", "numeric", "text", "text")) # Reading in the data from an excel file

ui <- dashboardPage(skin = "blue", # Making the header blue
                    
                    dashboardHeader(title = "Plotly Viz" # Title for the header
                    
                    ),
                    
                    dashboardSidebar( # Starting the dashboard sidebar
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              selectInput("Spread", h5(strong(em("MEDIUM OF SPREAD"))), # Declaring the input ID
                                                          
                                                          choices = c("All", unique(as.character(VicData$`Medium of Spread`))), # Creating unique choices for medium of spread
                                                          
                                                          selected = "All", multiple = TRUE)) # Enabling multi-select for this filter
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Disease", h5(strong(em("DISEASE"))), # Declaring the input ID
                                                          
                                                          choices = c("All", unique(as.character(VicData$Disease))), # Creating unique choices for diseases
                                                          
                                                          selected = "All", multiple = TRUE)) # Enabling multi-select for this filter
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Year", h5(strong(em("YEAR"))), # Declaring the input ID
                                                          
                                                          choices = c("All", unique(as.character(VicData$Year))), # Creating unique choices for year
                                                          
                                                          selected = "All", multiple = TRUE)) # Enabling multi-select for this filter
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Gender", h5(strong(em("GENDER"))), # Declaring the input ID
                                                          
                                                          choices = c("All", unique(as.character(VicData$Gender))), # Creating unique choices for gender
                                                          
                                                          selected = "All", multiple = TRUE)) # Enabling multi-select for this filter
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Category", h5(strong(em("AGE BRACKET"))), # Declaring the input ID
                                                          
                                                          choices = c("All", unique(as.character(VicData$Category))), # Creating unique choices for age bracket
                                                          
                                                          selected = "All", multiple = TRUE)) # Enabling multi-select for this filter
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Operand", h5(strong(em("SUMMARY STATISTIC"))), # Declaring the input ID
                                                          
                                                          choices = c("mean", "median", "sum", "sd", "IQR"), # Creating unique choices for summary statistic
                                                          
                                                          selected = "mean", multiple = FALSE)) # Disabling multi-select for this filter
                      )),
                      
                      
                      fluidRow(column(12, offset = 2, useShinyalert(), # Embedding Shinyalert UI launch
                                      
                                      actionButton(inputId = "PLOT", label = "PLOT", width = "40%", height = "40%", # Declaring the input ID, label and button text and color
                                                   
                                                   style = "color: #fff; background-color: #337ab7;border-color: #2e6da4")
                                      
                      )),
                      
                      fluidRow(column(12, div(style = "height:200px") # Leaving some space for the plot button
                                      
                      ))
                      
                      
                      ),
                    
                    dashboardBody( useShinyalert(), # Embedding Shinyalert UI launch
                      
                      tags$head(tags$style(HTML('
                                                .main-header .logo {
                                                font-family: "Georgia", Times,
                                                "Times New Roman",
                                                font-weight: bold;
                                                font-size: 24px;
                                                font-style: italic;
                                                }
                                                '))), # Declaring CSS for the font, font size for all the text in the application
                      
                      fluidRow(
                        
                        tabBox(height = "1100px", width = "1000px", # Declaring the page's dimensions
                               
                               tabPanel(title = tagList(icon("user", class = "fas fa-users"), "EXPLORATORY DATA ANALYSIS"), # Setting the tab's title and icon image
                                        
                                        box(title = "MEDIUM OF SPREAD VS GENDER",status = "primary",solidHeader = TRUE, # Creating a box, setting a solid header and plotting using the output render function
                                            
                                            collapsible = TRUE, plotlyOutput("id1")),
                                        
                                        box(title = "INFECTION TREND OVER THE LAST FEW YEARS",status = "primary",solidHeader = TRUE, # Creating a box, setting a solid header and plotting using the output render function
                                            
                                            collapsible = TRUE, plotlyOutput("id2")),
                                        
                                        box(plotlyOutput("id3"), status = "primary", solidHeader = TRUE, # Creating a box, setting a solid header and plotting using the output render function
                                            
                                            title = "DISTRIBUTION OF DISEASES", collapsible = TRUE),
                                        
                                        box(title = "DENSITY DISTRIBUTION OF MEDIUMS OF SPREAD",status = "primary",solidHeader = TRUE, # Creating a box, setting a solid header and plotting using the output render function
                                            
                                            collapsible = TRUE, plotlyOutput("id4"))
                               )
                               
                        )
                      )
                      ))

server <- function(input, output, session) {
  
    shinyalert(title = "INSTRUCTIONS TO USE", text = "Choose the desired Filters and then click on PLOT on the left side!", 
             type = "info", showConfirmButton = TRUE, confirmButtonText = "GOT IT!",
             confirmButtonCol = "#4CA3DD", timer = 40000) # Enabling the Shinyalert function to pop-up as soon as the application loads
  
    observeEvent(input$PLOT, { # Creating a reactive event which will get trigerred everytime plot is clicked 
    
    
    Spread_filtered <- input$Spread # Fetching user's input for medium of spread
    
    Disease_filtered <- input$Disease # Fetching user's input for disease
    
    Year_filtered <- input$Year # Fetching user's input for year
    
    Gender_filtered <- input$Gender # Fetching user's input for gender
    
    Category_filtered <- input$Category # Fetching user's input for age bracket
    
    
    VicData_Filtered <- VicData # Creating another dataset from the original one
    
    
    if(Spread_filtered != "All") { # Filtering data based on medium of spread
      
      VicData_Filtered <- VicData %>% filter(`Medium of Spread` == Spread_filtered) # Assigning the filtered data to VicData_Filtered
      
    }
    
    if(Disease_filtered != "All") { # Filtering data based on disease
      
      VicData_Filtered <- VicData %>% filter(Disease == Disease_filtered) # Assigning the filtered data to VicData_Filtered
      
    }
    
    if(Year_filtered != "All") { # Filtering data based on year
      
      VicData_Filtered <- VicData %>% filter(Year == Year_filtered) # Assigning the filtered data to VicData_Filtered
      
    }
    
    if(Gender_filtered != "All") { # Filtering data based on gender
      
      VicData_Filtered <- VicData %>% filter(Gender == Gender_filtered) # Assigning the filtered data to VicData_Filtered
      
    }
    
    if(Category_filtered != "All") { # Filtering data based on age bracket
      
      VicData_Filtered <- VicData %>% filter(Category == Category_filtered) # Assigning the filtered data to VicData_Filtered
      
    }
     
    
    
    if(nrow(VicData) == 0) { # Checking if our dataset has become exhausted
      
      shinyalert("Data not sufficient. Make some other selection!", type = "warning") # Triggering a warning through Shinyalert that data has been exhausted
      
    }
    
    else { # Getting into the else part, if everything is right
      
    output$id1 <- renderPlotly({ # Declaring a render plotly function to process plotly
      
      agg <- aggregate(Count ~ `Medium of Spread` + Gender, VicData_Filtered, FUN = input$Operand) # Aggregating cases based on spread and gender and using the user input for 'summary statistic' as the function
      
      gg_plot <- ggplot(agg, aes(x = Gender, y = Count, fill = `Medium of Spread`)) + geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
                 scale_fill_brewer(palette = "Dark2") + ylab("Count of Disease") + ggtitle(paste(toupper(as.character(input$Operand)), "Count of each Disease across both Genders")) + 
                 xlab(unique(as.character(VicData_Filtered$Gender))) + coord_flip() # Creating a ggplot2 bar chart object based on the aggregated data, labelling the axes and flipping the axis as well
      
      ggplotly(gg_plot) # Converting a ggplot2 onject to a plotly object
      
    })
      
      
    output$id2 <- renderPlotly({ # Declaring a render plotly function to process plotly
      
      agg <- aggregate(Count ~ Disease + Year, VicData_Filtered, FUN = input$Operand) # Aggregating cases based on disease and year and using the user input for 'summary statistic' as the function
      
      ggplot <- ggplot(aes(x = Year, y = Count, color = Disease), data = agg) + geom_line(size = 1, alpha = 0.7) + geom_point() + 
                scale_color_brewer(palette = "Dark2") + ylab("Count of Disease") + 
                ggtitle(paste(toupper(as.character(input$Operand)), "Count of each Disease across both Genders")) + 
                xlab(unique(as.character(VicData_Filtered$Year))) # Creating a ggplot2 line chart object based on the aggregated data, labelling the axes and using a dark theme
      
      ggplotly(ggplot) # Converting a ggplot2 onject to a plotly object
    })  
      
    output$id3 <- renderPlotly({ # Declaring a render plotly function to process plotly
      
      ggplot <- ggplot(aes(x = `Medium of Spread`, y = Count, color = Disease), data = VicData_Filtered) + geom_boxplot(size = 1, alpha = 0.7) + 
                scale_color_brewer(palette = "Dark2") + ylab("Count of Disease") + 
                ggtitle(paste(toupper(as.character(input$Operand)), "Count of each Disease across both Genders")) + 
                xlab(unique(as.character(VicData_Filtered$Disease))) # Creating a ggplot2 box plot object based on the data, labelling the axes and using a dark theme
      
      ggplotly(ggplot) # Converting a ggplot2 onject to a plotly object
      
    })  
      
    
    output$id4 <- renderPlotly({ # Declaring a render plotly function to process plotly
      
      ggplot <- ggplot(VicData_Filtered, aes(x = Count)) + geom_freqpoly(aes(color = `Medium of Spread`), alpha = 0.7, size = 1.3) + 
        scale_color_brewer(palette = "Dark2") + ylab("Distribution Density") + 
        ggtitle(paste(toupper(as.character(input$Operand)), "Distribution of each Mediums of Spread")) + xlim(-15, 40)
        xlab(unique(as.character(VicData_Filtered$`Medium of Spread`))) # Creating a ggplot2 density plot object based on the data, labelling the axes and using a dark theme
      
      ggplotly(ggplot) # Converting a ggplot2 onject to a plotly object
      
    })  
    
      
      
    }
    
  })
  
} # Closing the server side design

shinyApp(ui = ui, server = server) # Calling the function to launch the application

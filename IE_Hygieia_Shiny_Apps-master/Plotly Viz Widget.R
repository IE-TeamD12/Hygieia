library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readxl)
library(shinyalert)
library(viridis)
library(RColorBrewer)
library(viridisLite)

VicData <- read_excel("Victoria Infections Data.xlsx", col_types = c("text", "text", "numeric", "numeric", "text", "text"))

ui <- dashboardPage(skin = "blue", 
                    
                    dashboardHeader(title = "Plotly Viz"
                    
                    ),
                    
                    dashboardSidebar(
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              selectInput("Spread", h5(strong(em("MEDIUM OF SPREAD"))), 
                                                          
                                                          choices = c("All", unique(as.character(VicData$`Medium of Spread`))),
                                                          
                                                          selected = "All", multiple = TRUE))
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Disease", h5(strong(em("DISEASE"))),
                                                          
                                                          choices = c("All", unique(as.character(VicData$Disease))),
                                                          
                                                          selected = "All", multiple = TRUE))
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Year", h5(strong(em("YEAR"))),
                                                          
                                                          choices = c("All", unique(as.character(VicData$Year))),
                                                          
                                                          selected = "All", multiple = TRUE))
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Gender", h5(strong(em("GENDER"))),
                                                          
                                                          choices = c("All", unique(as.character(VicData$Gender))),
                                                          
                                                          selected = "All", multiple = TRUE))
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Category", h5(strong(em("AGE BRACKET"))),
                                                          
                                                          choices = c("All", unique(as.character(VicData$Category))),
                                                          
                                                          selected = "All", multiple = TRUE))
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px",
                                              
                                              selectInput("Operand", h5(strong(em("SUMMARY STATISTIC"))),
                                                          
                                                          choices = c("mean", "median", "sum", "sd", "IQR"),
                                                          
                                                          selected = "mean", multiple = FALSE))
                      )),
                      
                      
                      fluidRow(column(12, offset = 2, useShinyalert(),
                                      
                                      actionButton(inputId = "PLOT", label = "PLOT", width = "40%", height = "40%",
                                                   
                                                   style = "color: #fff; background-color: #337ab7;border-color: #2e6da4")
                                      
                      )),
                      
                      fluidRow(column(12, div(style = "height:200px")
                                      
                      ))
                      
                      
                      ),
                    
                    dashboardBody( useShinyalert(),
                      
                      tags$head(tags$style(HTML('
                                                .main-header .logo {
                                                font-family: "Georgia", Times,
                                                "Times New Roman",
                                                font-weight: bold;
                                                font-size: 24px;
                                                font-style: italic;
                                                }
                                                '))),
                      
                      fluidRow(
                        
                        tabBox(height = "1100px", width = "1000px",
                               
                               tabPanel(title = tagList(icon("user", class = "fas fa-users"), "EXPLORATORY DATA ANALYSIS"),
                                        
                                        box(title = "MEDIUM OF SPREAD VS GENDER",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotlyOutput("id1")),
                                        
                                        box(title = "INFECTION TREND OVER THE LAST FEW YEARS",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotlyOutput("id2")),
                                        
                                        box(plotlyOutput("id3"), status = "primary", solidHeader = TRUE,
                                            
                                            title = "DISTRIBUTION OF DISEASES", collapsible = TRUE),
                                        
                                        box(title = "DENSITY DISTRIBUTION OF MEDIUMS OF SPREAD",status = "primary",solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotlyOutput("id4"))
                               )
                               
                        )
                      )
                      ))

server <- function(input, output, session) {
  
  shinyalert(title = "INSTRUCTIONS TO USE", text = "Choose the desired Filters and then click on PLOT on the left side!", 
             type = "info", showConfirmButton = TRUE, confirmButtonText = "GOT IT!",
             confirmButtonCol = "#4CA3DD", timer = 40000)
  
  observeEvent(input$PLOT, {
    
    
    Spread_filtered <- input$Spread
    
    Disease_filtered <- input$Disease
    
    Year_filtered <- input$Year
    
    Gender_filtered <- input$Gender
    
    Category_filtered <- input$Category
    
    
    VicData_Filtered <- VicData
    
    
    if(Spread_filtered != "All") {
      
      VicData_Filtered <- VicData %>% filter(`Medium of Spread` == Spread_filtered)
      
    }
    
    if(Disease_filtered != "All") {
      
      VicData_Filtered <- VicData %>% filter(Disease == Disease_filtered)
      
    }
    
    if(Year_filtered != "All") {
      
      VicData_Filtered <- VicData %>% filter(Year == Year_filtered)
      
    }
    
    if(Gender_filtered != "All") {
      
      VicData_Filtered <- VicData %>% filter(Gender == Gender_filtered)
      
    }
    
    if(Category_filtered != "All") {
      
      VicData_Filtered <- VicData %>% filter(Category == Category_filtered)
      
    }
     
    
    
    if(nrow(VicData) == 0) {
      
      shinyalert("Data not sufficient. Make some other selection!", type = "warning")
      
    }
    
    else {
      
    output$id1 <- renderPlotly({
      
      agg <- aggregate(Count ~ `Medium of Spread` + Gender, VicData_Filtered, FUN = input$Operand)
      
      gg_plot <- ggplot(agg, aes(x = Gender, y = Count, fill = `Medium of Spread`)) + geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
                 scale_fill_brewer(palette = "Dark2") + ylab("Count of Disease") + ggtitle(paste(toupper(as.character(input$Operand)), "Count of each Disease across both Genders")) + 
                 xlab(unique(as.character(VicData_Filtered$Gender))) + coord_flip()
      
      ggplotly(gg_plot) 
      
    })
      
      
    output$id2 <- renderPlotly({
      
      agg <- aggregate(Count ~ Disease + Year, VicData_Filtered, FUN = input$Operand)
      
      ggplot <- ggplot(aes(x = Year, y = Count, color = Disease), data = agg) + geom_line(size = 1, alpha = 0.7) + geom_point() + 
                scale_color_brewer(palette = "Dark2") + ylab("Count of Disease") + 
                ggtitle(paste(toupper(as.character(input$Operand)), "Count of each Disease across both Genders")) + 
                xlab(unique(as.character(VicData_Filtered$Year)))
      
      ggplotly(ggplot)
    })  
      
    output$id3 <- renderPlotly({
      
      ggplot <- ggplot(aes(x = `Medium of Spread`, y = Count, color = Disease), data = VicData_Filtered) + geom_boxplot(size = 1, alpha = 0.7) + 
                scale_color_brewer(palette = "Dark2") + ylab("Count of Disease") + 
                ggtitle(paste(toupper(as.character(input$Operand)), "Count of each Disease across both Genders")) + 
                xlab(unique(as.character(VicData_Filtered$Disease)))
      
      ggplotly(ggplot)
      
    })  
      
    
    output$id4 <- renderPlotly({
      
      ggplot <- ggplot(VicData_Filtered, aes(x = Count)) + geom_freqpoly(aes(color = `Medium of Spread`), alpha = 0.7, size = 1.3) + 
        scale_color_brewer(palette = "Dark2") + ylab("Distribution Density") + 
        ggtitle(paste(toupper(as.character(input$Operand)), "Distribution of each Mediums of Spread")) + xlim(-15, 40)
        xlab(unique(as.character(VicData_Filtered$`Medium of Spread`)))
      
      ggplotly(ggplot)
      
    })  
    
      
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
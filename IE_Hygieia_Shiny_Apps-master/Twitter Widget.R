# THIS APPLICATION WILL LET THE USER SEARCH ANYTHING ON TWITTER (ALTHOUGH IT IS RECOMMENDED THAT THE USER SEARCHES ONLY FOR INFECTIONS)
# THIS WILL HELP THE USER UNDERSTAND WHAT ARE PEOPLE TALKING ABOUT ONLINE ABOUT ANYTHING

# THIS APPLICATION HAS BEEN DEVELOPED BY HYGIEIA, A PLATFORM FOR PREVENTING INFECTIONS AT THE WORKPLACE

# Initializing all packages

library(shinydashboard)
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(dashboardthemes)
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(purrr)
library(ggthemes)
library(tm)
library(RColorBrewer)
library(SnowballC)
library(wordcloud2)
library(ggpubr)
library(textdata)

app_name <- 'Twitter*sentiment*analysis' # Declaring Access Tokens to connect to our Twitter App
consumer_key <- 'GHxGo7sfUnGQFdbpe0L3bw7Kz'
consumer_secret <- 'WUC0ClQwrn2GePW46ZuxFgl28QpLVxws2Xzf6xCLtZG4w9jWvG'
access_token <- '4027888634-SKHsCu57vukIf1Rzzqy5IBd4HuJPramE5QQxbvx'
access_secret <- 'Eq1yrVJV305aM33WeNAzNaOKdGuS4HloLNyaeKrL155cP'

create_token(app = app_name, 
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token,
             access_secret = access_secret) # Creating Access Consumer Tokens

logo_turquoise_gradient <- shinyDashboardLogoDIY( # Making a customised header logo which wil appear on the top left part of the application
  
  boldText = ""
  ,mainText = ""
  ,textSize = 23
  ,badgeText = "TWITTER ANALYTICS" # Header text
  ,badgeTextColor = "white"
  ,badgeTextSize = 3.5
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 10
  
)

# UI side design

ui <- dashboardPage(skin = "blue", # Making the ehader blue
                    
                    dashboardHeader(title = logo_turquoise_gradient # Declaring header title
                                    
                                    
                                    
                                    
                    ),
                    
                    dashboardSidebar( # Initializing sidebar for the dashboard
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              searchInput(inputId = "TwitterSearch", label = "WHAT DO YOU WANT TO SEARCH?", # Declaring user input id and label
                                                          placeholder = "Search here", btnSearch = icon("search"), btnReset = icon("remove"))) # Declaring search options and placeholders and icons
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              selectInput(inputId = "TweetType", label = "TWEET TYPE", # Declaring input id and label
                                                          choices = c("popular", "mixed", "recent"), # Declaring the choices for the user
                                                          selected = "popular", multiple = FALSE)) # Disabling multi-select
                      )),
                      
                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #40E0D0}")),
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              sliderInput(inputId = "NumberOfTweets", label = "DESIRED NO. OF TWEETS", # Declaring the input id for tweets and the labels
                                                          min = 1, max = 300, value = 100, step = 1)) # Declaring the specifications for the slider input
                      )),
                      
                      
                      fluidRow(column(12, div(style = "height:200px") # Leaving some space after the slider input
                                      
                      ))
                      
                      
                    ),
                    
                    dashboardBody( useShinyalert(), shinyDashboardThemes(theme = "grey_dark"), # Enabling Shinyalert mechanism
                                   
                                   tags$head(tags$style(HTML('
                                                             .main-header .logo {
                                                             font-family: "Georgia", Times,
                                                             "Times New Roman",
                                                             font-weight: bold;
                                                             font-size: 24px;
                                                             font-style: italic;
                                                             }
                                                             '))), # Making a CSS class for the font and font size
                                   
                                   tags$style(HTML(" 
                                             .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                                             color: #ffffff;
                                             }
                                             .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}


                                             .dataTables_length select {
                                             color: #ffffff;
                                             background-color: #0E334A
                                             }


                                             .dataTables_filter input {
                                             color: #0E334A;
                                             background-color: #ffffff
                                             }

                                             thead {
                                             color: #ffffff;
                                             }

                                             tbody {
                                             color: #ffffff;
                                             }" # Custom CSS code to change the color of the 
                                                   
                                                   
                                   )),
                                   
                                   tags$style(type="text/css", # Making a custom CSS class to hide all errors popping up on the front end
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"
                                   ),
                                   
                                   fluidRow(
                                     
                                     tabBox(height = "1100px", width = "1000px", # Making the dimensions for the web page
                                            
                                            tabPanel(title = tagList(icon("user", class = "fas fa-project-diagram") # Fixing the icon for the tabpanel
                                                                     
                                                                     , "Tweet Feed Analytics"),
                                                     
                                                     box(title = "TWEETS WORDCLOUD", status = "primary", solidHeader = TRUE, 
                                                         
                                                         collapsible = TRUE, wordcloud2Output("TweetWordCloud", width = "98%"), width = 97), # Sending the wordcloud through the render function
                                                     
                                                     box(title = "TWEETS", status = "primary", solidHeader = TRUE,
                                                         
                                                         collapsible = TRUE, DT::dataTableOutput("Tweets"), width = 97) # Sending the table data output through a render finction
                                                     
                                                     
                                            )
                                            
                                     )
                                   )))

# Server side design

server <- function(input, output, session) {
  
  shinyalert(title = "WELCOME TO OUR TWITTER API!", text = paste("Enter the desired topic you want to search for on Twitter in the",
              strong("textbox on the left and hit the search icon!")), type = "info", showConfirmButton = TRUE, confirmButtonText = "GOT IT!",
             confirmButtonCol = "#40E0D0", closeOnClickOutside = TRUE, html = TRUE) # Shinyalert popup to show instruct the user on how to use the application
  
  
  output$TweetWordCloud <- renderWordcloud2({ # Render function for the wordcloud
    
    tweets_extracted <- search_tweets(input$TwitterSearch, n = input$NumberOfTweets, type = input$TweetType, include_rts = FALSE) # Searching for tweets on Twitter using user inputs
    
    tweets_extracted_dirty <- tweets_extracted %>% select(name, text, location) # Selecting only the relevant columns for analysis
    
    tweets_extracted_df <- as.data.frame(cbind(tweets_extracted_dirty$name, cbind(tweets_extracted_dirty$text, tweets_extracted_dirty$location))) # Combining columns for analysis
    
    colnames(tweets_extracted_df) <- c("Name", "Tweet", "Location") # Renaming columns
    
    tweets_corpus <- Corpus(VectorSource(tweets_extracted_df$Tweet)) # Converting the dataframe into a Vector Corpus
    
    removeSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) # Removing whitespaces
    
    tweets_corpus <- tm_map(tweets_corpus, removeSpace, "/") # Removing slashes
    
    tweets_corpus <- tm_map(tweets_corpus, removeSpace, "@") # Removing @ symbols
    
    tweets_corpus <- tm_map(tweets_corpus, removeSpace, "\\|") # Removing | symbols
    
    tweets_corpus <- tm_map(tweets_corpus, content_transformer(tolower)) # Converting everything to lower case
    
    tweets_corpus <- tm_map(tweets_corpus, removeNumbers) # Removing numbers
    
    tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english")) # Removing stopwords from the data
    
    tweets_corpus <- tm_map(tweets_corpus, removeWords, c("https", "tco")) # Removing tco and https words as they keep popping up
    
    tweets_corpus <- tm_map(tweets_corpus, removePunctuation) # Removing all punctuations
    
    tweets_corpus <- tm_map(tweets_corpus, stripWhitespace) # Removing whitespaces again
    
    tweets_corpus_tdm <- TermDocumentMatrix(tweets_corpus) # Converting the corpus in to a term-document-matrix
    
    tweets_corpus_tdm_matrix <- as.matrix(tweets_corpus_tdm) # Converting the tdm into a matrix for calculations
    
    tweets_corpus_tdm_matrix_sorted <- sort(rowSums(tweets_corpus_tdm_matrix), decreasing = TRUE) # Sorting words based on frequency
    
    tweets_wordcloud <- data.frame(word = names(tweets_corpus_tdm_matrix_sorted), freq = tweets_corpus_tdm_matrix_sorted) # Creating a dataframe
    
    wordcloud2(data = tweets_wordcloud, size = 1.5, color = "random-dark", backgroundColor = "black") # Making the wordcloud with a dark theme
    
  })
  
  output$Tweets <- DT::renderDataTable({
    
    tweets_extracted <- search_tweets(input$TwitterSearch, n = input$NumberOfTweets, type = input$TweetType, include_rts = FALSE) # # Searching for tweets on Twitter using user inputs
    
    tweets_extracted_dirty <- tweets_extracted %>% select(name, text, location) # Selecting only the relevant columns for analysis
    
    tweets_extracted_df <- as.data.frame(cbind(tweets_extracted_dirty$name, cbind(tweets_extracted_dirty$text, tweets_extracted_dirty$location))) # Combining columns for analysis
    
    colnames(tweets_extracted_df) <- c("Name", "Tweet", "Location") # Renaming columns
    
    tweets_extracted_df # Returning tweets data as a data frame
    
  })
  
  
  
  
} # Closing server side

shinyApp(ui = ui, server = server) # Launching Shiny app

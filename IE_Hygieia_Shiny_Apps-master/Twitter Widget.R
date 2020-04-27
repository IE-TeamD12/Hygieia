library(shinydashboard)
library(shiny)
library(shinyalert)
library(shinyWidgets)
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
library(processx)

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

ui <- dashboardPage(skin = "blue", 
                    
                    dashboardHeader(title = "Twitter Widget"
                                    
                                    
                                    
                                    
                    ),
                    
                    dashboardSidebar(
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              searchInput(inputId = "TwitterSearch", label = "Infections People are talking about",
                                                          placeholder = "COVID-19", btnSearch = icon("search"), btnReset = icon("remove")))
                      )),
                      
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              selectInput(inputId = "TweetType", label = "Tweet Type",
                                                          choices = c("popular", "mixed", "recent"),
                                                          selected = "popular", multiple = FALSE))
                      )),
                    
                      fluidRow(column(12, div(style = "height:100px", 
                                              
                                              sliderInput(inputId = "NumberOfTweets", label = "Desired No. of Tweets",
                                                          min = 1, max = 1000, value = 100, step = 1))
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
                        
                        tabBox(height = "1000px", width = "1000px",
                               
                               tabPanel(title = tagList(icon("user", class = "fas fa-project-diagram")
                                                        
                                                        , "Tweet Feed Analytics"),
                                        
                                        box(title = "TWEET WORDCLOUD", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, wordcloud2Output("TweetWordCloud"), width = 97),
                                        
                                        box(title = "TWEET COUNT", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("TweetCount"), width = 97),
                                        
                                        box(title = "TWEET SENTIMENT", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("TweetSentiment"), width = 97),
                                        
                                        box(title = "TWEET PIE SENTIMENT", status = "primary", solidHeader = TRUE,
                                            
                                            collapsible = TRUE, plotOutput("PieSentiment"), width = 97)
                                        
                                        
                               )
                               
                        )
                      )))



server <- function(input, output, session) {
  
      shinyalert(title = "INSTRUCTIONS TO USE", text = "Enter the desired topic you want to search for on Twitter in the
                 textbox on the left!", type = "info", showConfirmButton = TRUE, confirmButtonText = "GOT IT!",
                 confirmButtonCol = "#4CA3DD", timer = 30000)
  
      output$TweetCount <- renderPlot({
          
         tweets_extracted <- search_tweets(input$TwitterSearch, n = input$NumberOfTweets, type = input$TweetType, include_rts = FALSE) # Searching for tweets on Twitter
        
         tweets_extracted_dirty <- tweets_extracted %>% select(name, text, location)
        
         tweets_extracted_df <- as.data.frame(cbind(tweets_extracted_dirty$name, cbind(tweets_extracted_dirty$location, tweets_extracted_dirty$text)))
        
         colnames(tweets_extracted_df) <- c("Name", "Location", "Tweet")
        
         tweets_extracted_dirty$stripped_Text <- gsub("http:\\S+", "", tweets_extracted_dirty$text)
        
         tweets_extracted_dirty_stemmed <- tweets_extracted_dirty %>% select(stripped_Text) %>% unnest_tokens(word, stripped_Text)
        
         cleaned_tweets <- tweets_extracted_dirty_stemmed %>% anti_join(stop_words)
        
         cleaned_tweets %>% count(word, sort = TRUE) %>% top_n(20) %>% mutate(word = reorder(word, n)) %>% 
                            ggplot(aes(x = word, y = n)) + geom_col() + xlab("Word") + ylab("Unique Word Count") + coord_flip() + theme_bw()
          
      })
      
      output$TweetSentiment <- renderPlot({
        
        tweets_extracted <- search_tweets(input$TwitterSearch, n = input$NumberOfTweets, type = input$TweetType, include_rts = FALSE) # Searching for tweets on Twitter
        
        tweets_extracted_dirty <- tweets_extracted %>% select(name, text, location)
        
        tweets_extracted_df <- as.data.frame(cbind(tweets_extracted_dirty$name, cbind(tweets_extracted_dirty$location, tweets_extracted_dirty$text)))
        
        colnames(tweets_extracted_df) <- c("Name", "Location", "Tweet")
        
        tweets_extracted_dirty$stripped_Text <- gsub("http:\\S+", "", tweets_extracted_dirty$text)
        
        tweets_extracted_dirty_stemmed <- tweets_extracted_dirty %>% select(stripped_Text) %>% unnest_tokens(word, stripped_Text)
        
        cleaned_tweets <- tweets_extracted_dirty_stemmed %>% anti_join(stop_words)
        
        cleaned_tweets_bing <- cleaned_tweets %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
        
        cleaned_tweets_bing %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word, n)) %>%
                                ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
                                labs(title = "Overall Tweets Sentiment", y = "Contribution to Sentiment") + coord_flip() + theme_bw()
        
    })
      
    output$TweetWordCloud <- renderWordcloud2({
      
      tweets_extracted <- search_tweets(input$TwitterSearch, n = input$NumberOfTweets, type = input$TweetType, include_rts = FALSE) # Searching for tweets on Twitter
      
      tweets_extracted_dirty <- tweets_extracted %>% select(name, text, location)
      
      tweets_extracted_df <- as.data.frame(cbind(tweets_extracted_dirty$name, cbind(tweets_extracted_dirty$location, tweets_extracted_dirty$text)))
      
      colnames(tweets_extracted_df) <- c("Name", "Location", "Tweet")
      
      tweets_corpus <- Corpus(VectorSource(tweets_extracted_df$Tweet))
      
      removeSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
      
      tweets_corpus <- tm_map(tweets_corpus, removeSpace, "/")
      
      tweets_corpus <- tm_map(tweets_corpus, removeSpace, "@")
      
      tweets_corpus <- tm_map(tweets_corpus, removeSpace, "\\|")
      
      tweets_corpus <- tm_map(tweets_corpus, content_transformer(tolower))
      
      tweets_corpus <- tm_map(tweets_corpus, removeNumbers)
      
      tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english"))
      
      tweets_corpus <- tm_map(tweets_corpus, removeWords, c("https", "tco"))
      
      tweets_corpus <- tm_map(tweets_corpus, removePunctuation)
      
      tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)
      
      tweets_corpus_tdm <- TermDocumentMatrix(tweets_corpus)
      
      tweets_corpus_tdm_matrix <- as.matrix(tweets_corpus_tdm)
      
      tweets_corpus_tdm_matrix_sorted <- sort(rowSums(tweets_corpus_tdm_matrix), decreasing = TRUE)
      
      tweets_wordcloud <- data.frame(word = names(tweets_corpus_tdm_matrix_sorted), freq = tweets_corpus_tdm_matrix_sorted)
      
      wordcloud2(data = tweets_wordcloud, size = 1.5, color = "random-dark", backgroundColor = "black")
      
    })
    
  output$PieSentiment <- renderPlot({
    
    tweets_extracted <- search_tweets(input$TwitterSearch, n = input$NumberOfTweets, type = input$TweetType, include_rts = FALSE) # Searching for tweets on Twitter
    
    tweets_extracted_dirty <- tweets_extracted %>% select(name, text, location)
    
    tweets_extracted_df <- as.data.frame(cbind(tweets_extracted_dirty$name, cbind(tweets_extracted_dirty$location, tweets_extracted_dirty$text)))
    
    colnames(tweets_extracted_df) <- c("Name", "Location", "Tweet")
    
    tweets_extracted_dirty$stripped_Text <- gsub("http:\\S+", "", tweets_extracted_dirty$text)
    
    tweets_extracted_dirty_stemmed <- tweets_extracted_dirty %>% select(stripped_Text) %>% unnest_tokens(word, stripped_Text)
    
    cleaned_tweets <- tweets_extracted_dirty_stemmed %>% anti_join(stop_words)
    
    tweet_sentiment <- get_sentiments("nrc")
    
    nrc_words <- cleaned_tweets %>% inner_join(tweet_sentiment, by = "word")
    
    pie_words<- nrc_words %>% group_by(sentiment) %>% tally %>% arrange(desc(n))
    
    ggpie(pie_words, "n", label = "sentiment", 
          fill = "sentiment", color = "white", 
          palette = "Spectral", size = 1)
    
  })
 
  
}

shinyApp(ui = ui, server = server)
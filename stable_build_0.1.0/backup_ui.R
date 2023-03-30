library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(bslib)
library(waiter)

waiting_screen <- tagList(
  spin_plus(),
  h4("Initialising")) 

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "united"),
  useWaiter(),
  waiterShowOnLoad(html = waiting_screen, color = "#333e48"),
  navbarPage(
    "OpinionOasis",
    tabPanel(
      "Trends",
      HTML('<center><img src="https://icons.iconarchive.com/icons/xenatt/minimalism/256/App-Twitter-icon.png"></center>'),
      titlePanel(h1("Trending Twitter Topics", align = "center")),
      h3(""),
      wellPanel(selectInput("twitterTrendLocation", "",choices = list("Malaysia" = "Malaysia"), selected = "Malaysia")),
      DT::dataTableOutput("twitter_table_trends")
    ),
    tabPanel(
      "Twitter",
      verticalLayout(
        HTML('<center><img src="https://icons.iconarchive.com/icons/xenatt/minimalism/256/App-Twitter-icon.png"></center>'),
        titlePanel(h1("Twitter Sentiment Analysis", align = "center")),
        h3("Query"),
        wellPanel(
          #---------------------------------------------------------------------
          textInput("twitterKeyword","Query Keywords",""),
          sliderInput("twitterQueryNumber","Maximum number of Tweets",min = 1, max = 5000, value = 500),
          selectInput("twitterType","Tweet Type", choices = list("Recently Posted" = "recent", "Popular" = "popular", "Mixed" = "mixed"), selected = "mixed"),
          selectInput("twitterLanguage","Tweet Language", choices = list("English" = "en", "Malay" = "my"), selected = "en"),
          checkboxInput("twitterIncludeLinks","Include Links ", value = FALSE),
          checkboxInput("twitterIncludeReplies","Include Replies ", value = FALSE),
          checkboxInput("twitterIncludeRTS","Include Retweets ", value = FALSE),
          #---------------------------------------------------------------------
        ), # wellPanel
        
        h3("Filter"),
        wellPanel(
          #---------------------------------------------------------------------
          sliderInput("twitterMinimumLikes","Minimum Likes",min = 0, max = 100, value = 0),
          sliderInput("twitterMinimumReplies","Minimum Replies",min = 0, max = 100, value = 0),
          sliderInput("twitterMinimumRetweets","Minimum Retweets",min = 0, max = 100, value = 0),
          dateRangeInput("dates","Date Range"),
          
          actionButton("twitterAnalyze","Analyze"),
          #---------------------------------------------------------------------
        ), # wellPanel 
        
        h3("Results"),
        tabsetPanel(
          tabPanel(
            "Extraction",
            #---------------------------------------------------------------------
            h4("Extracted Data Summary"),
            tableOutput("twitter_table_extract_summary"),
            
            h4("Extracted Tweets"),
            DT::dataTableOutput("twitter_table_extract"),
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Combined", 
            #---------------------------------------------------------------------
            h4("Histogram of Sentiment Score overlaid over Average Twitter Sentiment Score"),
            plotlyOutput("twitter_iplot_oh_count_sentimentscore"),
            
            h4("Word Frequency Plot"),
            plotlyOutput("twitter_iplot_bc_word_freq"),
            
            h4("Grouped Bar Chart of Sentiment Count over Time (Day)"),
            plotlyOutput("twitter_iplot_gbc_sentiment_date"),
            
            h4("Stacked Bar Chart of Sentiment Count over Time (Day)"),
            plotlyOutput("twitter_iplot_sbc_sentiment_date"),
            
            h4("Grouped Bar Chart of Sentiment Count over Time (Hour)"),
            plotlyOutput("twitter_iplot_gbc_sentiment_datehour"),
            
            h4("Stacked Bar Chart of Sentiment Count over Time (Hour)"),
            plotlyOutput("twitter_iplot_sbc_sentiment_datehour"),
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Positive", 
            #---------------------------------------------------------------------
            "This panel is intentionally left blank"
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Neutral", 
            #---------------------------------------------------------------------
            "This panel is intentionally left blank"
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Negative", 
            #---------------------------------------------------------------------
            "This panel is intentionally left blank"
            #---------------------------------------------------------------------
          ),
        ) # tabsetPanel
      ), # verticalLayout
    ), # tabPanel "Twitter"
    
    tabPanel(
      "Reddit",
      sidebarLayout(
        sidebarPanel(
          actionButton("tiktokExtract","Extract"),
          actionButton("tiktokAnalyze","Analyze"),
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Tab 1",
              "This panel is intentionally left blank"
            ),
            tabPanel(
              "Tab 2", 
              "This panel is intentionally left blank"
            ),
            tabPanel(
              "Tab 3", 
              "This panel is intentionally left blank"
            ),
          )
        )
      )
    ),
    
    tabPanel(
      "YouTube",
      sidebarLayout(
        sidebarPanel(
          actionButton("youtubeExtract","Extract"),
          actionButton("youtubeAnalyze","Analyze"),
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Tab 1",
              "This panel is intentionally left blank"
            ),
            tabPanel(
              "Tab 2", 
              "This panel is intentionally left blank"
            ),
            tabPanel(
              "Tab 3", 
              "This panel is intentionally left blank"
            ),
          )
        )
      )
    )
  )
)
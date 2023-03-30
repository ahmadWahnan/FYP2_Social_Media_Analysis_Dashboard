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
          textInput("twitterKeyword","Search terms",""),
          sliderInput("twitterQueryNumber","Maximum number of tweets",min = 1, max = 15000, value = 100),
          selectInput("twitterType","Tweet type", choices = list("Recently Posted" = "recent", "Popular" = "popular", "Mixed" = "mixed"), selected = "mixed"),
          selectInput("twitterLanguage","Tweet language", choices = list("English" = "en", "Malay" = "my"), selected = "en"),
          checkboxInput("twitterIncludeLinks","Include links ", value = TRUE),
          checkboxInput("twitterIncludeReplies","Include replies ", value = TRUE),
          checkboxInput("twitterIncludeRTS","Include retweets ", value = FALSE),
          #---------------------------------------------------------------------
        ), # wellPanel
        
        h3("Filter"),
        wellPanel(
          #---------------------------------------------------------------------
          sliderInput("twitterMinimumLikes","Minimum Likes",min = 0, max = 100, value = 0),
          sliderInput("twitterMinimumReplies","Minimum Replies",min = 0, max = 100, value = 0),
          sliderInput("twitterMinimumRetweets","Minimum Retweets",min = 0, max = 100, value = 0),
          dateRangeInput("dates","Date Range"),
          
          #checkboxGroupInput("topic_type", label = "Topic Type", choices = c("Vehicles, Cars and Automobiles", "Beauty, Cosmetics and Body Care", "Art, Books and Literature", "Business, Marketing and Policies", "Careers, Skills and Jobs", "Education, Schools and Universities", "Events, Holidays and Festivals", "Family, Children and Parenting", "Cuisine, Food and Beverages", "Video Games, Multiplayer and Consoles", "Physical Health, Mental Health and Wellbeing", "Hobbies, Interests and Communities", "Home, Interior Decorating and Landscaping", "Law, Government, and Politics", "Videos, Movies and Television", "Music, Podcasts and Artists", "Personal Finance, Banking, Investments and Real Estate", "Cats, Dogs and Pets", "Science, Biology, Chemistry and Physics", "Society, Campaigns and Awareness", "Sports, Olympics and Leagues", "Accessories, Apparel, Style and Fashion", "Technology, Computers, Web Development and Programming", "Travel, Adventures and Landmarks", "Personal Thoughts, Opinions and Anecdotes", "Other"), selected = c("Vehicles, Cars and Automobiles", "Beauty, Cosmetics and Body Care", "Art, Books and Literature", "Business, Marketing and Policies", "Careers, Skills and Jobs", "Education, Schools and Universities", "Events, Holidays and Festivals", "Family, Children and Parenting", "Cuisine, Food and Beverages", "Video Games, Multiplayer and Consoles", "Physical Health, Mental Health and Wellbeing", "Hobbies, Interests and Communities", "Home, Interior Decorating and Landscaping", "Law, Government, and Politics", "Videos, Movies and Television", "Music, Podcasts and Artists", "Personal Finance, Banking, Investments and Real Estate", "Cats, Dogs and Pets", "Science, Biology, Chemistry and Physics", "Society, Campaigns and Awareness", "Sports, Olympics and Leagues", "Accessories, Apparel, Style and Fashion", "Technology, Computers, Web Development and Programming", "Travel, Adventures and Landmarks", "Personal Thoughts, Opinions and Anecdotes", "Other")),
          checkboxInput("twitter_include_topic_vehicles","Vehicles, Cars and Automobiles", value = TRUE),
          checkboxInput("twitter_include_topic_beauty","Beauty, Cosmetics and Body Care", value = TRUE),
          checkboxInput("twitter_include_topic_art","Art, Books and Literature", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Art, Books and Literature", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Business, Marketing and Policies", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Careers, Skills and Jobs", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Education, Schools and Universities", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Events, Holidays and Festivals", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Family, Children and Parenting", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Cuisine, Food and Beverages", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Video Games, Multiplayer and Consoles", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Physical Health, Mental Health and Wellbeing", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Hobbies, Interests and Communities", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Home, Interior Decorating and Landscaping", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Law, Government, and Politics", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Videos, Movies and Television", value = TRUE),
          checkboxInput("twitter_include_topic_vehicles","Cuisine, Food and Beverages", value = TRUE),
          
          actionButton("twitterAnalyze","Analyze"),
          #---------------------------------------------------------------------
        ), # wellPanel 
        
        h3("Results"),
        tabsetPanel(
          tabPanel(
            "Dataset",
            #---------------------------------------------------------------------
            h4("Dataset Summary"),
            tableOutput("twitter_table_extract_summary"),
            
            h4("Mention and Engagement Trend"),
            plotlyOutput("twitter_stacked_cumulative_percent_mention_engagement_by_hour"),
            
            h4("Stacked Mention and Engagement (Hour)"),
            plotlyOutput("twitter_stacked_mention_engagement_by_hour"),
            
            h4("Stacked Mention and Engagement (Day)"),
            plotlyOutput("twitter_stacked_mention_engagement_by_day"),
            
            h4("Dataset"),
            DT::dataTableOutput("twitter_table_extract"),
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "All", 
            #---------------------------------------------------------------------
            h4("Bigram Network Visualisation"),
            forceNetworkOutput("twitter_bigram_network_visualisation"),
            
            h4("Sankey Network Visualisation"),
            sankeyNetworkOutput("twitter_sankey_network_visualisation"),
            
            h4("Most Common Word"),
            plotlyOutput("twitter_common_1gram"),
            
            h4("Most Common Word Pair"),
            plotlyOutput("twitter_common_2gram"),
            
            h4("Most Common Trigram"),
            plotlyOutput("twitter_common_3gram"),
            
            h4("Sentiment Frequency"),
            plotlyOutput("twitter_common_sentiment"),
            
            h4("Phrase Frequency"),
            plotlyOutput("twitter_common_phrase"),
            
            h4("Emotion Frequency"),
            plotlyOutput("twitter_common_emotion"),
            
            h4("Topic Frequency"),
            plotlyOutput("twitter_common_topic"),
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Positive", 
            #---------------------------------------------------------------------
            h4("Bigram Network Visualisation"),
            forceNetworkOutput("twitter_bigram_network_visualisation_positive"),
            
            h4("Sankey Network Visualisation"),
            sankeyNetworkOutput("twitter_sankey_network_visualisation_positive"),
            
            h4("Most Common Word"),
            plotlyOutput("twitter_common_1gram_positive"),
            
            h4("Most Common Word Pair"),
            plotlyOutput("twitter_common_2gram_positive"),
            
            h4("Most Common Trigram"),
            plotlyOutput("twitter_common_3gram_positive"),
            
            h4("Phrase Frequency"),
            plotlyOutput("twitter_common_phrase_positive"),
            
            h4("Emotion Frequency"),
            plotlyOutput("twitter_common_emotion_positive"),
            
            h4("Topic Frequency"),
            plotlyOutput("twitter_common_topic_positive"),
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Neutral", 
            #---------------------------------------------------------------------
            h4("Bigram Network Visualisation"),
            forceNetworkOutput("twitter_bigram_network_visualisation_neutral"),
            
            h4("Sankey Network Visualisation"),
            sankeyNetworkOutput("twitter_sankey_network_visualisation_neutral"),
            
            h4("Most Common Word"),
            plotlyOutput("twitter_common_1gram_neutral"),
            
            h4("Most Common Word Pair"),
            plotlyOutput("twitter_common_2gram_neutral"),
            
            h4("Most Common Trigram"),
            plotlyOutput("twitter_common_3gram_neutral"),
            
            h4("Phrase Frequency"),
            plotlyOutput("twitter_common_phrase_neutral"),
            
            h4("Emotion Frequency"),
            plotlyOutput("twitter_common_emotion_neutral"),
            
            h4("Topic Frequency"),
            plotlyOutput("twitter_common_topic_neutral"),
            #---------------------------------------------------------------------
          ),
          tabPanel(
            "Negative", 
            #---------------------------------------------------------------------
            h4("Bigram Network Visualisation"),
            forceNetworkOutput("twitter_bigram_network_visualisation_negative"),
            
            h4("Sankey Network Visualisation"),
            sankeyNetworkOutput("twitter_sankey_network_visualisation_negative"),
            
            h4("Most Common Word"),
            plotlyOutput("twitter_common_1gram_negative"),
            
            h4("Most Common Word Pair"),
            plotlyOutput("twitter_common_2gram_negative"),
            
            h4("Most Common Trigram"),
            plotlyOutput("twitter_common_3gram_negative"),
            
            h4("Phrase Frequency"),
            plotlyOutput("twitter_common_phrase_negative"),
            
            h4("Emotion Frequency"),
            plotlyOutput("twitter_common_emotion_negative"),
            
            h4("Topic Frequency"),
            plotlyOutput("twitter_common_topic_negative"),
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
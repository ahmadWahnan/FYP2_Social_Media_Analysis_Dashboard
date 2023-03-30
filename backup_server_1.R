# Libraries --------------------------------------------------------------------
library(shiny)             # Shiny App Library
library(rtweet)            # Twitter data extraction
library(stringr)           # String data pre-processing
library(sentimentr)        # Speed optimized valence shifter augmented dictionary lookup and data augmentation
library(textclean)         # Text augmentation
library(ggplot2)           # Data Visualization
library(RColorBrewer)      # Nice data visualization color palette
library(hrbrthemes)        # Nice DV themes
library(vader)             # Social-Media-oriented rule based valence aware dictionary lookup
library(schrute)           # ditto above but output in data frame
library(dplyr)             # Data manipulation
library(SentimentAnalysis) # Standard dictionary lookup based off dictionaries
library(SnowballC)         # Shinyapp.io requirement to display data(?)
library(tidyr)             # Data cleaning and pre-processing
library(textclean)         # Text cleaning
library(sentiment.ai)      # Tensorflow AI Sentiment Analysis on the 'r-sentiment-ai' miniconda virtual enviroment
library(remotes)           # Download directly from GitHub
library(reticulate)        # Interface with Python
library(lubridate)         # Create date-time variable
library(ggthemes)          # DV Themes
library(plotly)            # Interactive Data Visualisation
library(truncnorm)         # Generate normal distribution for comparison with current data
library(wordcloud)         # Wordcloud DV
library(waiter)            # Show loading screen during AI model initialisation

# Setup ------------------------------------------------------------------------

# Initialise the sentiment ai model
# Must be initiated before starting calling sentiment.ai
# Model en.large is default; considerable resource use but most accurate for EN Lang
# Alternative models are...

init_sentiment.ai(model = "en.large",envname = "r-sentiment-ai")

# Create Twitter Auth Token 'auth'
api_key <- "MVUHrOfQKKrNvlC7UFofmo8jv"                               # Enter your consumer key
api_secret <- "aiNVfEcfjBSZ617MLIQI3S7LyNSr3BB75BiRzpIq7mMoZT6mPX"   # Enter your consumer secret
access_token <- "1109345608689565696-iSZBmSbCm1aAppHI5gT2iVB8giMZn7" # Enter your access token
access_secret <- "D7ZUD7HAKnW5q6OMekmByGqsdJwEbwLsS5u33zniicTVJ"     # Enter your access secret
auth <- rtweet_bot(api_key, api_secret, access_token, access_secret)

# Functions --------------------------------------------------------------------

# Text Cleaning function
clean_tweet_text <- function(df) {
  df <- gsub("#\\S+", "", df)    # Remove hashtags
  df <- gsub("@\\S+", "", df)    # Remove mentions
  df <- gsub("http\\S+", "", df) # Remove URLs
  df <- gsub("www\\S+", "", df)  # Remove links
  df <- replace_non_ascii(df)    # Remove non ascii characters (Emojis, etc.)
  return(df)
}

#===============================================================================
# Server Logic
#===============================================================================

# Main Server Logic
shinyServer(function(input, output) {
  
  # Twitter Section
  #-----------------------------------------------------------------------------
  twitterTrends <- get_trends("Malaysia", token = auth)
  output$twitter_table_trends <- DT::renderDataTable(
    subset(twitterTrends, select = c(trend,tweet_volume,place)),
    options = list(paging = FALSE))
  waiter_hide()
  
  # Begin twitterAnalyze process once "Analyze" button in Twitter navbar is pressed
  observeEvent(input$twitterAnalyze,{
    
    # Begin twitterAnalyze progress bar
    withProgress(message = 'Twitter Data Analyze Progress', value = 0, {
      twitterAnalyzeProgressSteps <- 9 # number of progress steps, update as you increase progress milestones in this reactive container
      
      # Input Search Parameters
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Recieving twitter search parameters"))
      twitterKeyword <- input$twitterKeyword
      twitterQueryNumber <- input$twitterQueryNumber
      twitterMinimumLikes <- input$twitterMinimumLikes
      twitterMinimumRetweets <- input$twitterMinimumRetweets
      twitterMinimumReplies <- input$twitterMinimumReplies
      twitterType <- input$twitterType
      twitterLanguage <- input$twitterLanguage
      twitterIncludeLinks <- input$twitterIncludeLinks
      twitterIncludeReplies <- input$twitterIncludeReplies
      twitterIncludeRTS <- input$twitterIncludeRTS
      
      # Augment search query based on parameters given
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Augmenting search query"))
      searchQuery <- twitterKeyword
      searchQuery <- paste(searchQuery,"")
      if (twitterMinimumLikes > 0) {searchQuery <- paste(searchQuery," min_faves:", twitterMinimumLikes, sep = "")}
      if (twitterMinimumRetweets > 0) {searchQuery <- paste(searchQuery," min_retweets:", twitterMinimumRetweets, sep = "")}
      if (twitterMinimumReplies > 0) {searchQuery <- paste(searchQuery," min_replies:", twitterMinimumReplies, sep = "")}
      if (!twitterIncludeLinks) {searchQuery <- paste(searchQuery,"-filter:links")}
      if (!twitterIncludeReplies) {searchQuery <- paste(searchQuery,"-filter:replies")}
      
      # Retrieving twitter query based of parameters given and append into main data frame
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Extracting tweets from Twitter API. This may take a while"))
      twitterQueryDF <- search_tweets2(searchQuery,                             # Search term for tweets augmented from 'inputKeyword'
                                       n = twitterQueryNumber,                  # Number of tweets to retrieve e.g. 50
                                       result_type = twitterType,               # Type of tweets e.g. recent, popular, mixed
                                       lang = twitterLanguage,                  # Tweet Language e.g. en
                                       include_rts = twitterIncludeRTS,         # Include re tweets e.g. TRUE, FALSE
                                       token = auth,                            # Authentication token bot
                                       parse = TRUE,                            # Tidy data frame
                                       retryonratelimit = TRUE,                 # Wait until rate limit refresh to retrieve tweets
                                       verbose = TRUE)
      
      # Data Reduction and Tidying
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Data Reduction and Tidying"))
      twitterDF <- subset(twitterQueryDF, select = c(created_at,text,lang,favorite_count,retweet_count)) # Subset query dataframe into working dataframe
      twitterDF['engagement'] <- twitterQueryDF$favorite_count+twitterQueryDF$retweet_count              # Create engagement column based off favoutites + retweets
      twitterDF <- twitterDF %>% separate(created_at,c("date","time"), sep = " ")                        # Split created_at into date and time
      twitterDF <- twitterDF %>% separate(date,c("year","month","day"), sep = "-")                       # Split date into year, month and day
      twitterDF <- twitterDF %>% separate(time,c("hour","minute","second"), sep = ":")                   # Split time into hour, minute and second
      
      # Data Cleaning
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Data Cleaning"))
      twitterDF['cleaned_text'] <- clean_tweet_text(twitterDF$text)
      
      # Running sentiment analysis via Sentiment AI
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Analyzing tweet sentiment"))
      twitterDF['sentimentai_score'] <- sentiment_score(twitterDF$cleaned_text)
      twitterDF['sentimentai_sentiment'] <- convertToDirection(twitterDF$sentimentai_score)
      
      # Data transformation for data analysis
      #-------------------------------------------------------------------------
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Transforming dataframe for data visualisation"))
      # Convert the year, month, day, and hour fields to a single datehour field
      twitterDF$datehour <- as.POSIXct(paste(twitterDF$year, twitterDF$month, twitterDF$day, twitterDF$hour), format = "%Y %m %d %H")
      
      # Convert the year, month and day fields to a single date field
      twitterDF$date <- as.POSIXct(paste(twitterDF$year, twitterDF$month, twitterDF$day), format = "%Y %m %d")
      
      # create dataframe containing count of sentiment by 1 hour buckets
      twitterDF_hts <- twitterDF %>% group_by(datehour, sentimentai_sentiment) %>% summarize(count = n())
      twitterTDF_hts <- twitterDF_hts %>% spread(sentimentai_sentiment, count) # spread the sentimentai_sentiment column
      
      # create dataframe containing count of engagement by 1 day buckets
      twitterDF_dts <- twitterDF %>% group_by(date, sentimentai_sentiment) %>% summarize(count = n())
      twitterTDF_dts <- twitterDF_dts %>% spread(sentimentai_sentiment, count) # spread the sentimentai_sentiment column
      
      # Data Visualization
      #-------------------------------------------------------------------------
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Data Visualization"))
      
      # Creating and output twitter data summary table
      twitterTable <- data.frame(
        Fields = c("Tweets",
                   "Likes",
                   "Retweets"),
        Total = c(length(twitterDF$text),
                  sum(twitterDF$favorite_count),
                  sum(twitterDF$retweet_count)),
        Minimum = c(" ",
                    min(twitterDF$favorite_count),
                    min(twitterDF$retweet_count)),
        Median = c(" ",
                   median(twitterDF$favorite_count),
                   median(twitterDF$retweet_count)),
        Mean = c(" ",
                 mean(twitterDF$favorite_count),
                 mean(twitterDF$retweet_count)),
        Maximum = c(" ",
                    max(twitterDF$favorite_count),
                    max(twitterDF$retweet_count)))
      output$twitter_table_extract_summary <- renderTable(twitterTable)
      
      # Output word chart
      #text_vector <- as.character(unique(twitterDF$cleaned_text))
      #words <- unlist(strsplit(text_vector, " "))
      #words <- words[words != ""]
      #words <- words[!(words %in% c("a", "&","able", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "tis", "to", "too", "twas", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your", "A", "Able", "About", "Across", "After", "All", "Almost", "Also", "Am", "Among", "An", "And", "Any", "Are", "As", "At", "Be", "Because", "Been", "But", "By", "Can", "Cannot", "Could", "Dear", "Did", "Do", "Does", "Either", "Else", "Ever", "Every", "For", "From", "Get", "Got", "Had", "Has", "Have", "He", "Her", "Hers", "Him", "His", "How", "However", "I", "If", "In", "Into", "Is", "It", "Its", "Just", "Least", "Let", "Like", "Likely", "May", "Me", "Might", "Most", "Must", "My", "Neither", "No", "Nor", "Not", "Of", "Off", "Often", "On", "Only", "Or", "Other", "Our", "Own", "Rather", "Said", "Say", "Says", "She", "Should", "Since", "So", "Some", "Than", "That", "The", "Their", "Them", "Then", "There", "These", "They", "This", "Tis", "To", "Too", "Twas", "Us", "Wants", "Was", "We", "Were", "What", "When", "Where", "Which", "While", "Who", "Whom", "Why", "Will", "With", "Would", "Yet", "You", "Your"))]
      #word_count <- sort(table(words), decreasing = TRUE)
      #twitterPlot <- plot_ly(y = names(head(sort(word_count, decreasing = TRUE), 20)), x = head(sort(word_count, decreasing = TRUE), 20), type = "bar", yaxis = list(type = "category", categoryorder = "frequency"), xaxis = list(type = "category"), orientation = 'h')
      #output$twitter_iplot_bc_word_freq <- renderPlotly(twitterPlot)
      
      text_vector <- as.character(unique(twitterDF$cleaned_text))
      words <- unlist(strsplit(text_vector, " "))
      words <- mgsub(as.character(twitterKeyword), "", words, ignore.case = TRUE, fixed = FALSE)
      words <- words[!(words %in% c(",",".","a", "&","able", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "tis", "to", "too", "twas", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your", "A", "Able", "About", "Across", "After", "All", "Almost", "Also", "Am", "Among", "An", "And", "Any", "Are", "As", "At", "Be", "Because", "Been", "But", "By", "Can", "Cannot", "Could", "Dear", "Did", "Do", "Does", "Either", "Else", "Ever", "Every", "For", "From", "Get", "Got", "Had", "Has", "Have", "He", "Her", "Hers", "Him", "His", "How", "However", "I", "If", "In", "Into", "Is", "It", "Its", "Just", "Least", "Let", "Like", "Likely", "May", "Me", "Might", "Most", "Must", "My", "Neither", "No", "Nor", "Not", "Of", "Off", "Often", "On", "Only", "Or", "Other", "Our", "Own", "Rather", "Said", "Say", "Says", "She", "Should", "Since", "So", "Some", "Than", "That", "The", "Their", "Them", "Then", "There", "These", "They", "This", "Tis", "To", "Too", "Twas", "Us", "Wants", "Was", "We", "Were", "What", "When", "Where", "Which", "While", "Who", "Whom", "Why", "Will", "With", "Would", "Yet", "You", "Your"))]
      words <- words[words != ""]
      word_count <- sort(table(words), decreasing = TRUE)
      twitterPlot <- plot_ly(y = names(head(sort(word_count, decreasing = TRUE), 30)), x = head(sort(word_count, decreasing = TRUE), 30), type = "bar", yaxis = list(type = "category", categoryorder = "frequency"), xaxis = list(type = "category"), orientation = 'h')
      output$twitter_iplot_bc_word_freq <- renderPlotly(twitterPlot)
      
      # Output raw extracted tweets
      output$twitter_table_extract <- DT::renderDataTable(
        subset(twitterQueryDF, select = c(created_at,text,favorite_count,retweet_count)),
        options = list(paging = FALSE))
      
      # Interactive overlay histogram of sentiment.ai score over average 
      iplot_oh_count_sentimentscore <- plot_ly(twitterDF, alpha = 0.25) # control colour opacity
      iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% add_histogram(x = ~sentimentai_score, bingroup=1, alpha = 1, color = I("#00BFC4"), name = 'Keyword-containing Tweets Sentiment Score')
      iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% add_histogram(x = ~rtruncnorm(n=nrow(twitterDF)/2, a=0, b=0.8, mean=0.7, sd=0.5), color = I("#619CFF"), bingroup=1, name = 'Average Positive Twitter Sentiment Score Distribution')
      iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% add_histogram(x = ~rtruncnorm(n=nrow(twitterDF)/2, a=-0.8, b=0, mean=-0.7, sd=0.5), color = I("#F8766D"), bingroup=1, name = 'Average Negative Twitter Sentiment Score Distribution')
      iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% layout(xaxis = list(title = "Sentiment Score"), yaxis = list(title = 'Count'), barmode = "overlay")
      output$twitter_iplot_oh_count_sentimentscore <- renderPlotly(iplot_oh_count_sentimentscore)
      
      # Interactive grouped bar chart of sentiment count over time (hour)
      iplot_gbc_sentiment_datehour <- plot_ly(twitterTDF_hts, x = ~datehour, y = ~positive, type = 'bar', color = I("#619CFF"), name = "Positive")
      if (sum(twitterTDF_hts$neutral) > 0) iplot_gbc_sentiment_datehour <- iplot_gbc_sentiment_datehour %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
      iplot_gbc_sentiment_datehour <- iplot_gbc_sentiment_datehour %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
      iplot_gbc_sentiment_datehour <- iplot_gbc_sentiment_datehour %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'group')
      output$twitter_iplot_gbc_sentiment_datehour <- renderPlotly(iplot_gbc_sentiment_datehour)
      
      # Interactive grouped bar chart of sentiment count over time (day)
      iplot_gbc_sentiment_date <- plot_ly(twitterTDF_dts, x = ~date, y = ~positive, color = I("#619CFF"), type = 'bar', name = "Positive")
      if (sum(twitterTDF_dts$neutral) > 0) iplot_gbc_sentiment_date <- iplot_gbc_sentiment_date %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
      iplot_gbc_sentiment_date <- iplot_gbc_sentiment_date %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
      iplot_gbc_sentiment_date <- iplot_gbc_sentiment_date %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'group')
      output$twitter_iplot_gbc_sentiment_date <- renderPlotly(iplot_gbc_sentiment_date)
      
      # Interactive stacked bar chart of sentiment count over time (hour)
      iplot_sbc_sentiment_datehour <- plot_ly(twitterTDF_hts, x = ~datehour, y = ~positive, color = I("#619CFF"), type = 'bar', name = "Positive")
      if (sum(twitterTDF_hts$neutral) > 0) iplot_sbc_sentiment_datehour <- iplot_sbc_sentiment_datehour %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
      iplot_sbc_sentiment_datehour <- iplot_sbc_sentiment_datehour %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
      iplot_sbc_sentiment_datehour <- iplot_sbc_sentiment_datehour %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'stack')
      output$twitter_iplot_sbc_sentiment_datehour <- renderPlotly(iplot_sbc_sentiment_datehour)
      
      # Interactive stacked bar chart of sentiment count over time (day)
      iplot_sbc_sentiment_date <- plot_ly(twitterTDF_dts, x = ~date, y = ~positive, color = I("#619CFF"), type = 'bar', name = "Positive")
      if (sum(twitterTDF_dts$neutral) > 0) iplot_sbc_sentiment_date <- iplot_sbc_sentiment_date %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
      iplot_sbc_sentiment_date <- iplot_sbc_sentiment_date %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
      iplot_sbc_sentiment_date <- iplot_sbc_sentiment_date %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'stack')
      output$twitter_iplot_sbc_sentiment_date <- renderPlotly(iplot_sbc_sentiment_date)
      
      # twitterAnalyze complete
      incProgress(1/twitterAnalyzeProgressSteps, detail = paste("Complete"))
      
    }) # end of twitterAnalyze withProgress progress bar 
    
  }) # end of twitterAnalyze observeEvent reactive container
  
}) # end of shinyServer logic
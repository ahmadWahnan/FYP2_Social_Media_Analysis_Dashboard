library(tidyjson)
library(networkD3)
library(tidyverse)
library(text2vec)
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


# Force install git version of reticulate due to SSL related bug in CRAN version
# one-time install only
#remotes::install_github("rstudio/reticulate")
#remotes::install_github("ropensci/rtweet")

# Installs miniconda for virtual environment deployment
#reticulate::install_miniconda()

# Creates a virtual enviroment "r-sentiment-ai" based on miniconda
# Every call for sentiment.ai functions is passed through here
# Python version must be 3.8.10 for compatibility reasons with required packages
# one-time creation only
#reticulate::virtualenv_create(envname = "r-sentiment-ai",python_version = "3.8.10")

# Setup virtual enviroment with packages required
# one-time setup only
#install_sentiment.ai()

# Initialise the sentiment ai model
# Must be initiated before starting calling sentiment.ai
# Model en.large is default; considerable resource use but most accurate for EN Lang
# Alternative models are...
init_sentiment.ai(model = "en.large",envname = "r-sentiment-ai")

api_key <- "MVUHrOfQKKrNvlC7UFofmo8jv"                               # Enter your consumer key
api_secret <- "aiNVfEcfjBSZ617MLIQI3S7LyNSr3BB75BiRzpIq7mMoZT6mPX"   # Enter your consumer secret
access_token <- "1109345608689565696-iSZBmSbCm1aAppHI5gT2iVB8giMZn7" # Enter your access token
access_secret <- "D7ZUD7HAKnW5q6OMekmByGqsdJwEbwLsS5u33zniicTVJ"     # Enter your access secret
bearer <- "AAAAAAAAAAAAAAAAAAAAANaJXwEAAAAAVKdVjfkRGS%2Fhy3J2ZX%2FJy9zOrTQ%3DS0W4nNoIX4xHoh14iXocGZ2MoD5ubqRpeLKI00FKyo3Y9WQ5O4" # Enter your bearer token
auth <- rtweet_bot(api_key, api_secret, access_token, access_secret)
token <- rtweet_app(bearer)

# Text Cleaning function
clean_tweet_text <- function(df) {
  df <- gsub("#\\S+", "", df)    # Remove hashtags
  df <- gsub("@\\S+", "", df)    # Remove mentions
  df <- gsub("http\\S+", "", df) # Remove URLs
  df <- gsub("www\\S+", "", df)  # Remove links
  df <- replace_non_ascii(df)    # Remove non ascii characters (Emojis, etc.)
  return(df)
}

convert_to_sentiment_type <- function(x) {
  # Check if x is a vector, matrix, or data.frame
  if (is.vector(x)) {
    # If x is a vector, convert values to "positive", "neutral", or "negative"
    x <- ifelse(x > 0.2833, "positive", ifelse(x < -0.2833, "negative", ifelse(x, "neutral")))
  } else if (is.matrix(x)) {
    # If x is a matrix, apply the function to each element of the matrix
    x <- apply(x, 1:3, convert_to_sentiment_type)
  } else if (is.data.frame(x)) {
    # If x is a data.frame, apply the function to each column of the data.frame
    x <- apply(x, 3, convert_to_sentiment_type)
  }
  return(x)
}

# Input Search Parameters
inputKeyword <- "windtrace -filter:retweets -filter:quote -filter:replies -filter:media -filter:verified OR filter:verified"
inputTweetQueryNumber <- 15000
inputType <- "recent"
inputLanguage <- "en"
inputIncludeRTS <- FALSE

#twitterStreamDF <- sample_stream(token = token, timeout = 50, fields = NULL, parse = FALSE)

#twitterStreamTDF <- twitterStreamDF %>% spread_all

twitterQueryDF <- search_tweets2(inputKeyword,                # Search term for tweets augmented from 'inputKeyword'
                                 n = inputTweetQueryNumber,    # Number of tweets to retrieve e.g. 50
                                 result_type = inputType,      # Type of tweets e.g. recent, popular, mixed
                                 lang = inputLanguage,         # Tweet Language e.g. en
                                 include_rts = inputIncludeRTS,# Include re tweets e.g. TRUE, FALSE
                                 token = auth,                 # Authentication token bot
                                 parse = TRUE,                 # Tidy data frame
                                 retryonratelimit = TRUE,      # Wait until rate limit refresh to retrieve tweets
                                 verbose = TRUE
)
#===============================================================================
# Drop columns and tidy created_at to discrete values for storage
twitterDF <- subset(twitterQueryDF, select = c(created_at,full_text,lang,favorite_count,retweet_count)) # Subset query dataframe into working dataframe

twitterDF['engagement'] <- twitterQueryDF$favorite_count+twitterQueryDF$retweet_count              # Create engagement column based off favoutites + retweets

twitterDF <- twitterDF %>% separate(created_at,c("date","time"), sep = " ")                        # Split created_at into date and time
twitterDF <- twitterDF %>% separate(date,c("year","month","day"), sep = "-")                       # Split date into year, month and day
twitterDF <- twitterDF %>% separate(time,c("hour","minute","second"), sep = ":")                   # Split time into hour, minute and second

# Convert the year, month, day, and hour fields to a single datehour field
twitterDF$datehour <- as.POSIXct(paste(twitterDF$year, twitterDF$month, twitterDF$day, twitterDF$hour), format = "%Y %m %d %H")

# Convert the year, month and day fields to a single date field
twitterDF$date <- as.POSIXct(paste(twitterDF$year, twitterDF$month, twitterDF$day), format = "%Y %m %d")

#===============================================================================

# Text Cleaning
#twitterDF['cleaned_text'] <- clean_tweet_text(twitterDF$full_text)
twitterDF['cleaned_text'] <- clean_tweets(twitterQueryDF, clean = c("users", "hashtags", "urls", "media"))

#=====[Sentiment Analysis]======================================================
# VADER....might be deprecated
#vaderDF <- vader_df(twitterDF$cleaned_text)
#twitterDF['vader_compound'] <- data.frame(vaderDF$compound)
#twitterDF['vader_positive'] <- data.frame(vaderDF$pos)
#twitterDF['vader_neutral'] <- data.frame(vaderDF$neu)
#twitterDF['vader_negative'] <- data.frame(vaderDF$neg)
#twitterDF['vader_sentiment'] <- convertToDirection(vaderDF$compound)

# Sentiment.ai, ensure init has run
twitterDF['sentimentai_score'] <- sentiment_score(twitterDF$cleaned_text)
summary(twitterDF$sentimentai_score)

#twitterDF['sentimentai_sentiment'] <- convertToDirection(twitterDF$sentimentai_score)
twitterDF['sentimentai_sentiment'] <- convert_to_sentiment_type(twitterDF$sentimentai_score)

# Data Visualization Playground
#===============================================================================
#twitterDF_backup <- twitterDF
#run this if you fucked up
twitterDF <- twitterDF_backup

#-------------------------------------------------------------------------------

# create dataframes based of sentiment type
twitterDF_positive <- twitterDF %>% filter(sentimentai_sentiment == 'positive')
twitterDF_neutral <- twitterDF %>% filter(sentimentai_sentiment == 'neutral')
twitterDF_negative <- twitterDF %>% filter(sentimentai_sentiment == 'negative')

# create dataframe containing count of sentiment by 1 hour buckets
twitterDF_hts <- twitterDF %>% group_by(datehour, sentimentai_sentiment) %>% summarize(count = n()) %>% spread(sentimentai_sentiment, count)

# create dataframe containing count of engagement by 1 day buckets
twitterDF_dts <- twitterDF %>% group_by(date, sentimentai_sentiment) %>% summarize(count = n()) %>% spread(sentimentai_sentiment, count)

#-------------------------------------------------------------------------------

# Simple Barchart
ggplot(twitterDF, aes(x = sentimentai_sentiment, fill = sentimentai_sentiment)) +
  geom_bar(stat = "count") +
  labs(title = "Sentiment by Count", x="Sentiment", y="Count") +
  scale_fill_manual("Sentiment Type",values = c("#F8766D", "#00BFC4", "#619CFF")) +
  theme_hc()

# time series stacked bar chart of Count of Tweets by Sentiment over Time
ggplot(twitterDF, aes(x = datehour, fill = sentimentai_sentiment)) +
  stat_count(aes(y = ..count..)) +
  scale_fill_manual("Sentiment Type",values = c("#F8766D", "#00BFC4", "#619CFF")) +
  scale_x_datetime(date_breaks = "1 day") +
  labs(title = "Count of Tweets by Sentiment over Time", x="Time", y="Count") +
  theme_hc()

# time series bar chart of Count of Tweet Engagement over Time
ggplot(twitterDF, aes(x = datehour, y = engagement)) +
  scale_x_datetime(date_breaks = "1 day") +
  geom_bar(stat = "identity", fill = "#00BFC4") +
  labs(title="Count of Tweet Engagement over Time", x="Time", y="Engagement") +
  theme_hc()

# time series line chart of Count of Sentiment by Time (Hour)
ggplot(twitterDF_hts, aes(x = datehour, y = count, color = sentimentai_sentiment)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day") +
  labs(title="Count of Sentiment by Time", x="Time", y="Count") +
  theme_hc()

# time series area chart of Count of Tweets by Sentiment over Time (Hour)
ggplot(twitterDF_hts, aes(x = datehour, y = count, fill = sentimentai_sentiment)) +
  geom_area() +
  scale_x_datetime(date_breaks = "1 day") +
  labs(title="Count of Tweets by Sentiment over Time", x="Time", y="Count") +
  theme_hc()

# time series line chart of Count of Sentiment by Time (Day)
ggplot(twitterDF_dts, aes(x = date, y = count, color = sentimentai_sentiment)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day") +
  labs(title="Count of Sentiment by Time", x="Time", y="Count") +
  theme_hc()

# time series area chart of Count of Tweets by Sentiment over Time (Day)
ggplot(twitterDF_dts, aes(x = date, y = count, fill = sentimentai_sentiment)) +
  geom_area() +
  scale_x_datetime(date_breaks = "1 day") +
  labs(title="Count of Tweets by Sentiment over Time", x="Time", y="Count") +
  theme_hc()

#-------------------------------------------------------------------------------

iplot_oh_count_sentimentscore <- plot_ly(twitterDF, alpha = 0.25) # control colour opacity
iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% add_histogram(x = ~sentimentai_score, bingroup=1, alpha = 1, color = I("#00BFC4"), name = 'Keyword-containing Tweets Sentiment Score')
iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% add_histogram(x = ~rtruncnorm(n=nrow(twitterDF)/2, a=0, b=0.8, mean=0.7, sd=0.5), color = I("#619CFF"), bingroup=1, name = 'Average Positive Twitter Sentiment Score Distribution')
iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% add_histogram(x = ~rtruncnorm(n=nrow(twitterDF)/2, a=-0.8, b=0, mean=-0.7, sd=0.5), color = I("#F8766D"), bingroup=1, name = 'Average Negative Twitter Sentiment Score Distribution')
iplot_oh_count_sentimentscore <- iplot_oh_count_sentimentscore %>% layout(xaxis = list(title = "Sentiment Score"), yaxis = list(title = 'Count'), barmode = "overlay")
iplot_oh_count_sentimentscore

iplot_gbc_sentiment_datehour <- plot_ly(twitterDF_hts, x = ~datehour, y = ~positive, type = 'bar', color = I("#619CFF"), name = "Positive")
if (sum(twitterTDF_hts$neutral) > 0) iplot_gbc_sentiment_datehour <- iplot_gbc_sentiment_datehour %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
iplot_gbc_sentiment_datehour <- iplot_gbc_sentiment_datehour %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
iplot_gbc_sentiment_datehour <- iplot_gbc_sentiment_datehour %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'group')
iplot_gbc_sentiment_datehour

iplot_gbc_sentiment_date <- plot_ly(twitterDF_dts, x = ~date, y = ~positive, color = I("#619CFF"), type = 'bar', name = "Positive")
if (sum(twitterTDF_dts$neutral) > 0) iplot_gbc_sentiment_date <- iplot_gbc_sentiment_date %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
iplot_gbc_sentiment_date <- iplot_gbc_sentiment_date %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
iplot_gbc_sentiment_date <- iplot_gbc_sentiment_date %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'group')
iplot_gbc_sentiment_date

iplot_sbc_sentiment_datehour <- plot_ly(twitterDF_hts, x = ~datehour, y = ~positive, color = I("#619CFF"), type = 'bar', name = "Positive")
if (sum(twitterTDF_hts$neutral) > 0) iplot_sbc_sentiment_datehour <- iplot_sbc_sentiment_datehour %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
iplot_sbc_sentiment_datehour <- iplot_sbc_sentiment_datehour %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
iplot_sbc_sentiment_datehour <- iplot_sbc_sentiment_datehour %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'stack')
iplot_sbc_sentiment_datehour

iplot_sbc_sentiment_date <- plot_ly(twitterDF_dts, x = ~date, y = ~positive, color = I("#619CFF"), type = 'bar', name = "Positive")
if (sum(twitterTDF_dts$neutral) > 0) iplot_sbc_sentiment_date <- iplot_sbc_sentiment_date %>% add_trace(y = ~neutral, color = I("#00BFC4"), name = 'Neutral')
iplot_sbc_sentiment_date <- iplot_sbc_sentiment_date %>% add_trace(y = ~negative, color = I("#F8766D"), name = 'Negative')
iplot_sbc_sentiment_date <- iplot_sbc_sentiment_date %>% layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Count'), barmode = 'stack')
iplot_sbc_sentiment_date

text_vector <- as.character(unique(twitterDF$cleaned_text))
words <- unlist(strsplit(text_vector, " "))
words <- gsub("windtrace", "", words, ignore.case = TRUE)
words <- words[!(words %in% c(",",".","a", "&","able", "about", "across", "after", "all", "almost", "also", "am", "among", "an", "and", "any", "are", "as", "at", "be", "because", "been", "but", "by", "can", "cannot", "could", "dear", "did", "do", "does", "either", "else", "ever", "every", "for", "from", "get", "got", "had", "has", "have", "he", "her", "hers", "him", "his", "how", "however", "i", "if", "in", "into", "is", "it", "its", "just", "least", "let", "like", "likely", "may", "me", "might", "most", "must", "my", "neither", "no", "nor", "not", "of", "off", "often", "on", "only", "or", "other", "our", "own", "rather", "said", "say", "says", "she", "should", "since", "so", "some", "than", "that", "the", "their", "them", "then", "there", "these", "they", "this", "tis", "to", "too", "twas", "us", "wants", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "would", "yet", "you", "your", "A", "Able", "About", "Across", "After", "All", "Almost", "Also", "Am", "Among", "An", "And", "Any", "Are", "As", "At", "Be", "Because", "Been", "But", "By", "Can", "Cannot", "Could", "Dear", "Did", "Do", "Does", "Either", "Else", "Ever", "Every", "For", "From", "Get", "Got", "Had", "Has", "Have", "He", "Her", "Hers", "Him", "His", "How", "However", "I", "If", "In", "Into", "Is", "It", "Its", "Just", "Least", "Let", "Like", "Likely", "May", "Me", "Might", "Most", "Must", "My", "Neither", "No", "Nor", "Not", "Of", "Off", "Often", "On", "Only", "Or", "Other", "Our", "Own", "Rather", "Said", "Say", "Says", "She", "Should", "Since", "So", "Some", "Than", "That", "The", "Their", "Them", "Then", "There", "These", "They", "This", "Tis", "To", "Too", "Twas", "Us", "Wants", "Was", "We", "Were", "What", "When", "Where", "Which", "While", "Who", "Whom", "Why", "Will", "With", "Would", "Yet", "You", "Your"))]
words <- words[words != ""]
word_count <- sort(table(words), decreasing = TRUE)
plot_ly(y = names(head(sort(word_count, decreasing = TRUE), 50)), x = head(sort(word_count, decreasing = TRUE), 50), type = "bar", yaxis = list(type = "category", categoryorder = "frequency"), xaxis = list(type = "category"), orientation = 'h')
wordcloud(words, min.freq = 5, max.words = 200, random.order = FALSE, rot.per = 0, colors = c("#619CFF", "#00BFC4", "#F8766D"))


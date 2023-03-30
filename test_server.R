library(tm)
library(tidyjson)
library(igraph)
library(stringi)
library(stopwords)
library(influential)
library(networkD3)
library(tidytext)
library(tidyverse)
library(shiny)             # Shiny App Library
library(rtweet)            # Twitter data extraction
library(stringr)           # String data pre-processing
library(ggplot2)           # Data Visualization
library(RColorBrewer)      # Nice data visualization color palette
library(hrbrthemes)        # Nice DV themes
library(dplyr)             # Data manipulation
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

# Initialise the sentiment ai model
init_sentiment.ai(model = "en.large",envname = "r-sentiment-ai")

# Setup Twitter API access token and bearer
api_key <- "MVUHrOfQKKrNvlC7UFofmo8jv"                               # Enter your consumer key
api_secret <- "aiNVfEcfjBSZ617MLIQI3S7LyNSr3BB75BiRzpIq7mMoZT6mPX"   # Enter your consumer secret
access_token <- "1109345608689565696-iSZBmSbCm1aAppHI5gT2iVB8giMZn7" # Enter your access token
access_secret <- "D7ZUD7HAKnW5q6OMekmByGqsdJwEbwLsS5u33zniicTVJ"     # Enter your access secret
bearer <- "AAAAAAAAAAAAAAAAAAAAANaJXwEAAAAAVKdVjfkRGS%2Fhy3J2ZX%2FJy9zOrTQ%3DS0W4nNoIX4xHoh14iXocGZ2MoD5ubqRpeLKI00FKyo3Y9WQ5O4" # Enter your bearer token
auth <- rtweet_bot(api_key, api_secret, access_token, access_secret)
token <- rtweet_app(bearer)

# Input Search Parameters
inputKeyword <- "chatgpt"
inputTweetQueryNumber <- 5000
inputType <- "mixed"
inputLanguage <- "en"
inputIncludeRTS <- FALSE

twitterQueryDF <- search_tweets(inputKeyword,                 # Search term for tweets augmented from 'inputKeyword'
                                n = inputTweetQueryNumber,    # Number of tweets to retrieve e.g. 50
                                result_type = inputType,      # Type of tweets e.g. recent, popular, mixed
                                lang = inputLanguage,         # Tweet Language e.g. en
                                include_rts = inputIncludeRTS,# Include re tweets e.g. TRUE, FALSE
                                token = auth,                 # Authentication token bot
                                parse = TRUE,                 # Tidy data frame
                                retryonratelimit = TRUE,      # Wait until rate limit refresh to retrieve tweets
                                verbose = TRUE
)

# Process extracted tweets
twitterDF <- process_twitter_data(twitterQueryDF)

# Text Cleaning
twitterDF['cleaned_text'] <- clean_tweet_text(twitterDF$full_text)

# Analyze Text
twitterDF <- analyze_text(twitterDF, twitterDF$cleaned_text)

# create dataframes based of sentiment type
twitterDF_positive <- twitterDF %>% filter(sentimentai_sentiment == 'positive')
twitterDF_neutral <- twitterDF %>% filter(sentimentai_sentiment == 'neutral')
twitterDF_negative <- twitterDF %>% filter(sentimentai_sentiment == 'negative')

# Create dataframe that summarize data in 1 hour bins
twitterDF_hour <- group_and_summarize_data_by_hour(twitterDF)

# Create dataframe that summarize data in 1 day bins
twitterDF_day <- group_and_summarize_data_by_day(twitterDF)

plot_bigram_network_visualisation(twitterDF$cleaned_text, 30)
plot_sankey_network_visualisation(twitterDF$cleaned_text, 30)
plot_common_ngram(twitterDF$cleaned_text, 30, inputKeyword, TRUE, 1)
plot_common_ngram(twitterDF$cleaned_text, 30, inputKeyword, TRUE, 2)
plot_common_ngram(twitterDF_negative$cleaned_text, 30, inputKeyword, TRUE, 3)
plot_common_ngram(twitterDF_negative$cleaned_text, 30, inputKeyword, TRUE, 4)
plot_common_ngram(twitterDF_negative$cleaned_text, 30, inputKeyword, TRUE, 5)

plot_stacked_mention_engagement_by_hour(twitterDF_hour)
plot_stacked_cumulative_mention_engagement_by_hour(twitterDF_hour)
plot_stacked_cumulative_percent_mention_engagement_by_hour(twitterDF_hour)

plot_common_ngram(twitterDF$cleaned_text, 30, inputKeyword, TRUE, 2)

# Un-functioned DV Code
#===============================================================================================================================================
#-------------------------------------------------------------------------------

plot_ly(twitterDF_hour, x = ~datehour, type = "bar", 
        y = ~negative_mention, color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, color = I("#619CFF"), alpha = 1, name = "Positive Mentions") %>%
  add_trace(y = ~negative_engagement, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") %>%
  layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Incidence'), barmode = "group")


plot_ly(twitterDF_day, x = ~date, type = "bar", 
        y = ~negative_mention, color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, color = I("#619CFF"), alpha = 1, name = "Positive Mentions") %>%
  add_trace(y = ~negative_engagement, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") %>%
  layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), yaxis = list(title = 'Incidence'), barmode = "group")

#-------------------------------------------------------------------------------

#-----------(Hour)-------------

# Mention and Engagement over Time (Hours) (Stacked Bar Chart and Stacked Filled Line Chart)
figure1 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") %>%
  layout(title = 'Mention by the Hour (Stacked)', xaxis = list(title = "Mentions"), barmode = "stack")

figure2 <- plot_ly(twitterDF_hour, x = ~datehour,  
                   y = ~negative_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(title = 'Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")

figure

# Cumulative Mention and Engagement over Time (Hours) (Stacked Bar Chart and Stacked Line Chart)
figure1 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(title = 'Cumulative Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")

figure

# Cumulative Percent (%) Mention and Engagement over Time (Hours) (Stacked Line Chart)
figure1 <- plot_ly(twitterDF_hour, x = ~datehour, groupnorm = 'percent',
                   y = ~negative_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_hour, x = ~datehour, groupnorm = 'percent',
                   y = ~negative_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(title = 'Cumulative Percent (%) Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")

figure

# Mention and Engagement over Time (Hours) (Grouped Bar Chart and Line Chart)
figure1 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_engagement, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(title = 'Mention and Engagement by the Hour (Grouped)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")

figure

# Cumulative Mention and Engagement over Time (Hours) (Grouped Bar Chart and Line Chart)
figure1 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_hour, x = ~datehour, 
                   y = ~negative_engagement_cumulative, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(title = 'Cumulative Mention and Engagement by the Hour (Grouped)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")

figure

#-----------(Day)-------------

# Mention and Engagement over Time (Day) (Stacked Bar Chart and Stacked Filled Line Chart)
figure1 <- plot_ly(twitterDF_day, x = ~date, 
                   y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_day, x = ~date, 
                   y = ~negative_engagement, type = "scatter", mode = "lines", stackgroup = 'one', color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", stackgroup = 'one', color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", stackgroup = 'one', color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")

figure

# Mention and Engagement over Time (Day) (Grouped Bar Chart and Line Chart)
figure1 <- plot_ly(twitterDF_day, x = ~date, 
                   y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_day, x = ~date, 
                   y = ~negative_engagement, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")

figure

# Cumulative Mention and Engagement over Time (Day) (Grouped Bar Chart and Line Chart)
figure1 <- plot_ly(twitterDF_day, x = ~date, 
                   y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

figure2 <- plot_ly(twitterDF_day, x = ~date, 
                   y = ~negative_engagement_cumulative, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 

figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")

figure
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



sentences <- as.character(unique(twitterDF$cleaned_text))                       # Create vector containing all unique text
keywords <- as.character(unique(inputKeyword))                                  # Create vector containing keyword(s)

words <- unlist(strsplit(sentences, " "))                                       # Create word tokens from sentence vector
keywords <- unlist(strsplit(keywords, " "))                                     # Create keyword tokens from keywords vector

words <- tolower(words)                                                         # Convert word tokens to lowercase
keywords <- tolower(keywords)                                                   # Convert keyword tokens to lowercase

words <- gsub("[[:punct:]]", "", words)                                         # Remove punctuation marks from word tokens
keywords <- gsub("[[:punct:]]", "", keywords)                                   # Remove punctuation marks from keyword tokens

words <- removeWords(words, keywords)

words <- words[!(words %in% stopwords("english"))]                              # Remove common stop words from word tokens
words <- gsub("[0-9]", "", words) 
words <- words[words != ""]                                                     # Remove empty tokens from word tokens

word_count <- sort(table(words), decreasing = TRUE)
plot_ly(y = names(head(sort(word_count, decreasing = TRUE), 30)), x = head(sort(word_count, decreasing = TRUE), 30), type = "bar", yaxis = list(type = "category", categoryorder = "frequency"), xaxis = list(type = "category"), orientation = 'h')
wordcloud(words, min.freq = 5, max.words = 200, random.order = FALSE, rot.per = 0, colors = c("#619CFF", "#00BFC4", "#F8766D"))

#-------------------------------------------------------------------------------

bigram_network_visualisation <- function(df) {
  
  # Create bigram dataframe from df
  bigram <- df %>% 
    select(cleaned_text) %>%
    unnest_tokens(bigram, cleaned_text, token = "ngrams", n = 2) %>%
    filter(! is.na(bigram))
  
  # Create stop word dataframe
  stopwordDF <- tibble(word = c(stopwords("en")))
  
  # Filter stop words and spaces from bigram
  bigram %<>% 
    separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
    filter(! word1 %in% stopwordDF$word) %>% 
    filter(! word2 %in% stopwordDF$word) %>% 
    filter(! is.na(word1)) %>% 
    filter(! is.na(word2))
  
  # Create bigram frequency that groups and count by bigram
  bigram_frequency <- bigram %>% 
    dplyr::count(word1, word2, sort = TRUE) %>% 
    dplyr::rename(weight = n)
  
  # Setting frequency threshold to value of top 100 of bigram_frequency
  threshold <- as.integer(bigram_frequency[100, 'weight'])
  
  # Create an undirected network graph object 
  network <-  bigram_frequency %>%
    filter(weight > threshold) %>%
    graph_from_data_frame(directed = FALSE)
  
  # Store the degree.
  V(network)$degree <- strength(graph = network)
  
  # Compute the weight shares.
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  
  # Create networkD3 object.
  networkD3 <- igraph_to_networkD3(g = network)
  
  # Define node size.
  networkD3$nodes <- networkD3$nodes %>% mutate(degree = (1E-2)*V(network)$degree)
  
  # Define color group
  networkD3$nodes <- networkD3$nodes %>% mutate(group = 1)
  
  # Define edges width. 
  networkD3$links$value <- 10*E(network)$width
  
  # Render forced network visualisation
  plot <- forceNetwork(
    Links = networkD3$links, 
    Nodes = networkD3$nodes, 
    Source = 'source', 
    Target = 'target',
    NodeID = 'name',
    Group = 'group', 
    opacity = 0.9,
    Value = 'value',
    fontFamily = "arial",
    Nodesize = 'degree', 
    linkWidth = JS("function(d) { return Math.sqrt(d.value)*1.5; }"), 
    radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"),
    linkColour = "#666",
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  )
  return(plot)
}



#-------------------------------------------------------------------------------

sentences <- as.character(unique(twitterDF$cleaned_text))                       # Create vector containing all unique text
sentences <- tolower(sentences)                                                 # Convert to lowercase
sentences <- gsub("[[:punct:]]", "", sentences)                                 # Remove punctuation 
sentences <- removeWords(sentences, stopwords("english"))                       # Remove common stop words
sentences <- sentences[sentences != ""]                                         # Remove empty sentences

# Create bigram dataframe from twitterDF
bigram <- as.data.frame(sentences) %>% 
  unnest_tokens(bigram, sentences, token = "ngrams", n = 2) %>%
  filter(! is.na(bigram))

# Filter stop words and spaces from bigram
bigram %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2))

# Create bigram frequency that groups and count by bigram
bigram_frequency <- bigram %>% 
  dplyr::count(word1, word2, sort = TRUE) %>% 
  dplyr::rename(weight = n)

# Setting frequency threshold to value of top 50 of bigram_frequency
threshold <- as.integer(bigram_frequency[100, 'weight'])

# Create an undirected network graph object 
network <-  bigram_frequency %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
networkD3 <- igraph_to_networkD3(g = network)

# Define node size.
networkD3$nodes <- networkD3$nodes %>% mutate(degree = (1E-2)*V(network)$degree)

# Define color group
networkD3$nodes <- networkD3$nodes %>% mutate(group = 1)

# Define edges width. 
networkD3$links$value <- 10*E(network)$width

# Render forced network visualisation
forceNetwork(
  Links = networkD3$links, 
  Nodes = networkD3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'group', 
  opacity = 0.9,
  Value = 'value',
  fontFamily = "arial",
  Nodesize = 'degree', 
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"),
  linkColour = "#666",
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

# Render sankey network visualisation
sankeyNetwork(
  Links = networkD3$links, 
  Nodes = networkD3$nodes, 
  Source = "source",
  Target = "target", 
  Value = "value", 
  NodeID = "name",
  fontFamily = "arial",
  fontSize = 12, 
  nodeWidth = 30
)



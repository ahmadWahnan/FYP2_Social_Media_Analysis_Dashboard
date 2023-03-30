# Setup Twitter API access token and bearer
api_key <- "MVUHrOfQKKrNvlC7UFofmo8jv"                               # Enter your consumer key
api_secret <- "aiNVfEcfjBSZ617MLIQI3S7LyNSr3BB75BiRzpIq7mMoZT6mPX"   # Enter your consumer secret
access_token <- "1109345608689565696-iSZBmSbCm1aAppHI5gT2iVB8giMZn7" # Enter your access token
access_secret <- "D7ZUD7HAKnW5q6OMekmByGqsdJwEbwLsS5u33zniicTVJ"     # Enter your access secret
bearer <- "AAAAAAAAAAAAAAAAAAAAANaJXwEAAAAAVKdVjfkRGS%2Fhy3J2ZX%2FJy9zOrTQ%3DS0W4nNoIX4xHoh14iXocGZ2MoD5ubqRpeLKI00FKyo3Y9WQ5O4" # Enter your bearer token
auth <- rtweet_bot(api_key, api_secret, access_token, access_secret)

twitterQueryDF <- search_tweets("chatgpt",                 # Search term for tweets augmented from 'inputKeyword'
                                n = 500,    # Number of tweets to retrieve e.g. 50
                                result_type = "mixed",      # Type of tweets e.g. recent, popular, mixed
                                lang = "en",         # Tweet Language e.g. en
                                include_rts = FALSE,# Include re tweets e.g. TRUE, FALSE
                                token = auth,                 # Authentication token bot
                                parse = TRUE,                 # Tidy data frame
                                retryonratelimit = TRUE,      # Wait until rate limit refresh to retrieve tweets
                                verbose = TRUE
)

# Data Reduction and Tidying
twitterDF <- process_twitter_data(twitterQueryDF)

# Data Cleaning
twitterDF['cleaned_text'] <- clean_tweet_text(twitterQueryDF$text)

# Running sentiment analysis via Sentiment AI
twitterDF <- analyze_text(twitterDF, twitterDF$cleaned_text)

# Data transformation for data analysis
#-------------------------------------------------------------------------
# create dataframes based of sentiment type
twitterDF_positive <- twitterDF %>% filter(sentiment == 'positive')
twitterDF_neutral <- twitterDF %>% filter(sentiment == 'neutral')
twitterDF_negative <- twitterDF %>% filter(sentiment == 'negative')

# Create dataframe that summarize data in 1 hour bins
twitterDF_hour <- group_and_summarize_data_by_hour(twitterDF)

# Create dataframe that summarize data in 1 day bins
twitterDF_day <- group_and_summarize_data_by_day(twitterDF)

# Data Visualization
#-------------------------------------------------------------------------
# Creating and output twitter data summary table
data.frame(
  Fields = c("Tweets",
             "Likes",
             "Retweets"),
  Total = c(length(twitterDF$full_text),
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
              max(twitterDF$retweet_count))
  )

plot_bigram_network_visualisation(twitterDF$cleaned_text, 30)
plot_sankey_network_visualisation(twitterDF$cleaned_text, 30)
plot_common_ngram(twitterDF$cleaned_text, 30, twitterKeyword, TRUE, 1)
plot_common_ngram(twitterDF$cleaned_text, 30, twitterKeyword, TRUE, 2)
plot_common_ngram(twitterDF_negative$cleaned_text, 30, twitterKeyword, TRUE, 3)
plot_common_ngram(twitterDF_negative$cleaned_text, 30, twitterKeyword, TRUE, 4)
plot_common_ngram(twitterDF_negative$cleaned_text, 30, twitterKeyword, TRUE, 5)

plot_stacked_mention_engagement_by_hour(twitterDF_hour)
plot_stacked_cumulative_mention_engagement_by_hour(twitterDF_hour)
plot_stacked_cumulative_percent_mention_engagement_by_hour(twitterDF_hour)

plot_common_ngram(twitterDF$cleaned_text, 30, twitterKeyword, TRUE, 2)


library(tidytext)
library(plotly)

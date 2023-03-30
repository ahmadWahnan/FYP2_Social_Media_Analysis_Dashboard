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
  return(x)}

clean_tweet_text <- function(df) {
  df <- gsub("#\\S+", "", df)    # Remove hashtags
  df <- gsub("@\\S+", "", df)    # Remove mentions
  df <- gsub("http\\S+", "", df) # Remove URLs
  df <- gsub("www\\S+", "", df)  # Remove links
  df <- gsub("&\\S+", "", df)    # Remove html tags
  df <- gsub("\\s+", " ", df)    # Remove unnecessary spaces
  df <- trimws(df, which = c("both")) # Remove trailing spaces
  df <- gsub("source:", "", ignore.case = TRUE,  df)    # Remove link prefixes
  return(df)}

process_twitter_data <- function(df) {
  # Subset the data frame to include only the created_at, full_text, lang, favorite_count, and retweet_count columns
  twitterDF <- subset(df, select = c(created_at,full_text,lang,favorite_count,retweet_count))
  # Create an engagement column based on the sum of the favorite_count and retweet_count columns
  twitterDF['engagement'] <- df$favorite_count+df$retweet_count
  # Split the created_at column into separate date and time columns
  twitterDF <- twitterDF %>% separate(created_at,c("date","time"), sep = " ")
  # Split the date column into separate year, month, and day columns
  twitterDF <- twitterDF %>% separate(date,c("year","month","day"), sep = "-")
  # Split the time column into separate hour, minute, and second columns
  twitterDF <- twitterDF %>% separate(time,c("hour","minute","second"), sep = ":")
  # Convert the year, month, day, and hour columns to a single datehour column
  twitterDF$datehour <- as.POSIXct(paste(twitterDF$year, twitterDF$month, twitterDF$day, twitterDF$hour), format = "%Y %m %d %H")
  # Convert the year, month, and day columns to a single date column
  twitterDF$date <- as.POSIXct(paste(twitterDF$year, twitterDF$month, twitterDF$day), format = "%Y %m %d")
  return(twitterDF)
}

# Analyze text using tensorflow model en.large via sentiment.ai
analyze_text <- function(df, text) {
  
  df_phrase <- sentiment_match(text)
  target_mx <- embed_text(text)
  
  topics <- c("Vehicles, Cars and Automobiles", "Beauty, Cosmetics and Body Care", "Art, Books and Literature", "Business, Marketing and Policies", "Careers, Skills and Jobs", "Education, Schools and Universities", "Events, Holidays and Festivals", "Family, Children and Parenting", "Cuisine, Food and Beverages", "Video Games, Multiplayer and Consoles", "Physical Health, Mental Health and Wellbeing", "Hobbies, Interests and Communities", "Home, Interior Decorating and Landscaping", "Law, Government, and Politics", "Videos, Movies and Television", "Music, Podcasts and Artists", "Personal Finance, Banking, Investments and Real Estate", "Cats, Dogs and Pets", "Science, Biology, Chemistry and Physics", "Society, Campaigns and Awareness", "Sports, Olympics and Leagues", "Accessories, Apparel, Style and Fashion", "Technology, Computers, Web Development and Programming", "Travel, Adventures and Landmarks", "Personal Thoughts, Opinions and Anecdotes", "Other")
  ref_mx_topic    <- embed_text(topics)
  df_topic <- cosine_match(target_mx, ref_mx_topic)
  df_topic <- df_topic %>% filter(rank == 1)
  
  emotion <- emotions <- c("Admiration", "Amusement", "Anger", "Annoyance", "Approval", "Caring", "Confusion", "Curiosity", "Desire", "Disappointment", "Disapproval", "Disgust", "Embarrassment", "Excitement", "Fear", "Gratitude", "Grief", "Joy", "Love", "Nervousness", "Optimism", "Pride", "Realization", "Relief", "Remorse", "Sadness", "Surprise", "Neutral")
  ref_mx_emotion    <- embed_text(emotion)
  df_emotion <- cosine_match(target_mx, ref_mx_emotion)
  df_emotion <- df_emotion %>% filter(rank == 1)
  
  df_new <- df
  df_new['sentiment'] <- convert_to_sentiment_type(df_phrase$sentiment)
  df_new['phrase'] <- df_phrase$phrase
  df_new['topic'] <- df_topic$reference
  df_new['emotion'] <- df_emotion$reference
  df_new['sentiment_score'] <- round(rescale(df_phrase$sentiment, to = c(-100, 100)),2)
  df_new['phrase_certainty'] <- round(rescale(df_phrase$similarity, to = c(0, 100)),2)
  df_new['topic_certainty'] <- round(rescale(df_topic$similarity, to = c(0, 100)),2)
  df_new['emotion_certainty'] <- round(rescale(df_emotion$similarity, to = c(0, 100)),2)
  
  return(df_new)
}

group_and_summarize_data_by_hour <- function(df) {
  df_hour <- df %>%
    group_by(datehour) %>%
    summarize(positive_mention = sum(sentiment == "positive"),
              neutral_mention = sum(sentiment == "neutral"),
              negative_mention = sum(sentiment == "negative"),
              mention = sum(positive_mention, neutral_mention, negative_mention),
              engagement = sum(engagement))
  df_positive <- df %>% filter(sentiment == "positive")
  df_neutral <- df %>% filter(sentiment == "neutral")
  df_negative <- df %>% filter(sentiment == "negative")
  df_hour <- full_join(df_hour, df_positive %>% group_by(datehour) %>% summarize(positive_engagement = sum(engagement)), by = "datehour")
  df_hour <- full_join(df_hour, df_neutral %>% group_by(datehour) %>% summarize(neutral_engagement = sum(engagement)), by = "datehour")
  df_hour <- full_join(df_hour, df_negative %>% group_by(datehour) %>% summarize(negative_engagement = sum(engagement)), by = "datehour")
  df_hour <- df_hour %>% replace(is.na(.), 0)
  df_hour <- df_hour %>% mutate(positive_mention_cumulative = cumsum(positive_mention), neutral_mention_cumulative = cumsum(neutral_mention), negative_mention_cumulative = cumsum(negative_mention))
  df_hour <- df_hour %>% mutate(positive_engagement_cumulative = cumsum(positive_engagement), neutral_engagement_cumulative = cumsum(neutral_engagement), negative_engagement_cumulative = cumsum(negative_engagement))
  return(df_hour)
}

group_and_summarize_data_by_day <- function(df) {
  df_day <- df %>%
    group_by(date) %>%
    summarize(positive_mention = sum(sentiment == "positive"),
              neutral_mention = sum(sentiment == "neutral"),
              negative_mention = sum(sentiment == "negative"),
              mention = sum(positive_mention, neutral_mention, negative_mention),
              engagement = sum(engagement))
  df_positive <- df %>% filter(sentiment == "positive")
  df_neutral <- df %>% filter(sentiment == "neutral")
  df_negative <- df %>% filter(sentiment == "negative")
  df_day <- full_join(df_day, df_positive %>% group_by(date) %>% summarize(positive_engagement = sum(engagement)), by = "date")
  df_day <- full_join(df_day, df_neutral %>% group_by(date) %>% summarize(neutral_engagement = sum(engagement)), by = "date")
  df_day <- full_join(df_day, df_negative %>% group_by(date) %>% summarize(negative_engagement = sum(engagement)), by = "date")
  df_day <- df_day %>% replace(is.na(.), 0)
  df_day <- df_day %>% mutate(positive_mention_cumulative = cumsum(positive_mention), neutral_mention_cumulative = cumsum(neutral_mention), negative_mention_cumulative = cumsum(negative_mention))
  df_day <- df_day %>% mutate(positive_engagement_cumulative = cumsum(positive_engagement), neutral_engagement_cumulative = cumsum(neutral_engagement), negative_engagement_cumulative = cumsum(negative_engagement))
  return(df_day)
}

# Bigram Network DV ------------------------------------------------------------
# Recieves text vector, return forced bigram network visualisation
# use renderForceNetwork() on server.r and forceNetworkOutput("") on ui.r 

# Return forced bigram network visualisation
plot_bigram_network_visualisation <- function(df, n) {
  
  sentences <- as.character(unique(df))                                           # Create vector containing all unique text
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
  threshold <- as.integer(bigram_frequency[n, 'weight'])
  
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
  
  # Store forced network visualisation into plot
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
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
    radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"),
    linkColour = "#666",
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  )
  
  # Return plot
  return(plot)
}

# Return sankey network visualisation
plot_sankey_network_visualisation <- function(df, n) {
  
  sentences <- as.character(unique(df))                                           # Create vector containing all unique text
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
  threshold <- as.integer(bigram_frequency[n, 'weight'])
  
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
  
  # Render sankey network visualisation
  plot <- sankeyNetwork(
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
  
  return(plot)
}

# N-gram Frequency DV ------------------------------------------------------------

# Plots n most common n-gram, allow stopwords
plot_common_ngram <- function(df, n, inputKeyword, removeStopwords, ngram_val) {
  
  keywords <- as.character(unique(inputKeyword))                                  # Create vector containing keyword(s)
  keywords <- unlist(strsplit(keywords, " "))                                     # Create keyword tokens from keywords vector
  keywords <- tolower(keywords)                                                   # Convert keyword tokens to lowercase
  keywords <- gsub("[[:punct:]]", "", keywords)                                   # Remove punctuation marks from keyword tokens
  
  sentences <- as.character(unique(df))                                           # Create vector containing all unique text
  sentences <- tolower(sentences)                                                 # Convert to lowercase
  sentences <- gsub("[[:punct:]]", "", sentences)                                 # Remove punctuation 
  sentences <- removeWords(sentences, keywords)                                   # Remove keyword tokens
  if(removeStopwords) sentences <- removeWords(sentences, stopwords("english"))   # Remove common stop words
  sentences <- sentences[sentences != ""]                                         # Remove empty sentences
  
  # Create n-gram dataframe from df
  ngram <- as.data.frame(sentences) %>% 
    unnest_tokens(ngram, sentences, token = "ngrams", n = ngram_val) %>%
    filter(! is.na(ngram))
  
  ngram_count <- sort(table(ngram), decreasing = TRUE)
  
  plot <- plot_ly(y = names(head(sort(ngram_count, decreasing = TRUE), n)), x = head(sort(ngram_count, decreasing = TRUE), n), type = "bar", yaxis = list(type = "category", categoryorder = "frequency"), xaxis = list(type = "category"), orientation = 'h')
  
  return(plot)
}

# Plot n most common categories
plot_common_categories <- function(df, n) {
  count <- sort(table(df), decreasing = TRUE)
  plot <- plot_ly(y = names(head(sort(count, decreasing = TRUE), n)), x = head(sort(count, decreasing = TRUE), n), type = "bar", yaxis = list(type = "category", categoryorder = "frequency"), xaxis = list(type = "category"), orientation = 'h')
  return(plot)
}

# Mention and Engagement Sentiment DV ------------------------------------------

#---------------(HOUR)-----------------

# Plots stacked mentions and engagements by the hour
plot_stacked_mention_engagement_by_hour <- function(df) {
  figure1 <- plot_ly(df, x = ~datehour, 
                     y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") %>%
    layout(title = 'Mention by the Hour (Stacked)', xaxis = list(title = "Mentions"), barmode = "stack")
  
  figure2 <- plot_ly(df, x = ~datehour,  
                     y = ~negative_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")
  
  return(figure)}

# Plot stacked cumulative mentions and engagements by the hour
plot_stacked_cumulative_mention_engagement_by_hour <- function(df) {
  # Create the first plot using the negative_mention_cumulative, neutral_mention_cumulative, and positive_mention_cumulative columns as y values
  figure1 <- plot_ly(df, x = ~datehour, 
                     y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 
  # Create the second plot using the negative_engagement_cumulative, neutral_engagement_cumulative, and positive_engagement_cumulative columns as y values
  figure2 <- plot_ly(df, x = ~datehour, 
                     y = ~negative_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 
  # Combine the two plots into a single figure with the first plot above the second plot
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Cumulative Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")
  return(figure)
}

# Plot stacked cumulative percent mentions and engagements by the hour
plot_stacked_cumulative_percent_mention_engagement_by_hour <- function(df) {
  figure1 <- plot_ly(df, x = ~datehour, groupnorm = 'percent',
                     y = ~negative_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Mentions") 
  
  figure2 <- plot_ly(df, x = ~datehour, groupnorm = 'percent',
                     y = ~negative_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Cumulative Percent (%) Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")
  return(figure)
}

# Mention and Engagement over Time (Hours) (Grouped Bar Chart and Line Chart)
plot_grouped_mention_engagement_by_hour <- function(df) {
  figure1 <- plot_ly(df, x = ~datehour, 
                   y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
  add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
  add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 

  figure2 <- plot_ly(df, x = ~datehour, 
                   y = ~negative_engagement, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
  add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
  add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 

  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
  layout(title = 'Mention and Engagement by the Hour (Grouped)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")
  return(figure)
}

# Cumulative Mention and Engagement over Time (Hours) (Grouped Bar Chart and Line Chart)
plot_grouped_cumulative_mention_engagement_by_hour <- function(df) {
  figure1 <- plot_ly(df, x = ~datehour, 
                     y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 
  
  figure2 <- plot_ly(df, x = ~datehour, 
                     y = ~negative_engagement_cumulative, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Cumulative Mention and Engagement by the Hour (Grouped)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")
  
  return(figure)
}

#---------------(DAY)------------------

# Plots stacked mentions and engagements by the day
plot_stacked_mention_engagement_by_day <- function(df) {
  figure1 <- plot_ly(df, x = ~date, 
                     y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") %>%
    layout(title = 'Mention by the Hour (Stacked)', xaxis = list(title = "Mentions"), barmode = "stack")
  
  figure2 <- plot_ly(df, x = ~date,  
                     y = ~negative_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")
  
  return(figure)}

# Plot stacked cumulative mentions and engagements by the day
plot_stacked_cumulative_mention_engagement_by_day <- function(df) {
  # Create the first plot using the negative_mention_cumulative, neutral_mention_cumulative, and positive_mention_cumulative columns as y values
  figure1 <- plot_ly(df, x = ~date, 
                     y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 
  # Create the second plot using the negative_engagement_cumulative, neutral_engagement_cumulative, and positive_engagement_cumulative columns as y values
  figure2 <- plot_ly(df, x = ~date, 
                     y = ~negative_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 
  # Combine the two plots into a single figure with the first plot above the second plot
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Cumulative Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")
  return(figure)
}

# Plot stacked cumulative percent mentions and engagements by the day
plot_stacked_cumulative_percent_mention_engagement_by_day <- function(df) {
  figure1 <- plot_ly(df, x = ~date, groupnorm = 'percent',
                     y = ~negative_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Mentions") 
  
  figure2 <- plot_ly(df, x = ~date, groupnorm = 'percent',
                     y = ~negative_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#F8766D", alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "lightgrey", alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "none", stackgroup = 'one', fillcolor = "#619CFF", alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Cumulative Percent (%) Mention and Engagement by the Hour (Stacked)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "stack")
  return(figure)
}

# Plots grouped mentions and line engagements by the day
plot_grouped_mention_engagement_by_day <- function(df) {
  figure1 <- plot_ly(df, x = ~date, 
                     y = ~negative_mention, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 
  
  figure2 <- plot_ly(df, x = ~date, 
                     y = ~negative_engagement, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Mention and Engagement by the Hour (Grouped)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")
  return(figure)
}

# Plots grouped cumulative mentions and line engagements by the day
plot_grouped_cumulative_mention_engagement_by_day <- function(df) {
  figure1 <- plot_ly(df, x = ~date, 
                     y = ~negative_mention_cumulative, type = "bar", color = I("#F8766D"), alpha = 1, name = "Negative Mentions") %>%
    add_trace(y = ~neutral_mention_cumulative, type = "bar", color = I("lightgrey"), alpha = 1, name = "Neutral Mentions") %>%
    add_trace(y = ~positive_mention_cumulative, type = "bar", color = I("#619CFF"), alpha = 1, name = "Positive Mentions") 
  
  figure2 <- plot_ly(df, x = ~date, 
                     y = ~negative_engagement_cumulative, type = "scatter", mode = "lines", color = I("#F8766D"), alpha = 1, name = "Negative Engagement") %>%
    add_trace(y = ~neutral_engagement_cumulative, type = "scatter", mode = "lines", color = I("lightgrey"), alpha = 1, name = "Neutral Engagement") %>%
    add_trace(y = ~positive_engagement_cumulative, type = "scatter", mode = "lines", color = I("#619CFF"), alpha = 1, name = "Positive Engagement") 
  
  figure <- subplot(figure1, figure2, nrows = 2, shareX = TRUE) %>%
    layout(title = 'Cumulative Mention and Engagement by the Hour (Grouped)', xaxis = list(title = "Time", rangeslider = list(type = "date")), barmode = "group")
  
  return(figure)
}


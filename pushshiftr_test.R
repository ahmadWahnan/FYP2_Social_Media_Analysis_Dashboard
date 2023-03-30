library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)


res <- httr::GET("https://api.pushshift.io/reddit/search/comment/?q=tenso&size=250")

result <- jsonlite::fromJSON(content(httr::GET("https://api.pushshift.io/reddit/search/comment/?q=tensorflow&size=250"), "text", encoding = "UTF-8"))

comments <- unique(rbind(comments, new_comments))

comments <- data.frame(author = result$data$author,                     
                       subreddit = result$data$subreddit,
                       text = result$data$body,
                       score = result$data$score,
                       datetime = result$data$utc_datetime_str,
                       permalink = result$data$permalink)

comments <- comments %>% filter(!grepl("*I am a bot, and this action was performed automatically.", comments$text))
comments['sentimentai_score'] <- sentiment_score(comments$text)

summary(comments)

plot_common_ngram(comments$text, 30, 'deep rock galactic', TRUE, 1)
plot_common_ngram(comments$subreddit, 30, '', FALSE, 1)
plot_common_ngram(comments$text, 30, 'deep rock galactic', TRUE, 2)
plot_common_ngram(comments$text, 30, 'deep rock galactic', FALSE, 3)

plot_sankey_network_visualisation(comments$text, 30)
plot_bigram_network_visualisation(comments$text, 30)


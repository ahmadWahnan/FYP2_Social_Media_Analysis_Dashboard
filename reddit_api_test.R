install.packages("RedditExtractoR")
library(RedditExtractoR)
library(jsonlite)
library(dplyr)

devtools::install_github("whereofonecannotspeak/pushshiftr")
library(pushshiftr)


thread_url <- find_thread_urls(
  keywords = "yelan",
  sort_by = "relevance",
  subreddit = NA,
  period = "month"
)

thread_content <- get_thread_content(thread_url$url)

comments <- thread_content$comments
threads <- thread_content$threads

plot_common_ngram(threads$title, 30, 'al haitham alhaitham alhaithams amp t', TRUE, 2)
plot_common_ngram(threads$text, 30, 'al haitham alhaitham alhaithams amp t', TRUE, 2)

plot_common_ngram(comments$comment, 30, 'yelan amp t s', TRUE, 2)

plot_sankey_network_visualisation(comments$comment, 30)
plot_bigram_network_visualisation(comments$comment, 30)

ps_search_comments(search_terms = 'alhaitham')
ps_search_submissions(search_terms = c("Cao Cao", "Liu Bei"), subreddit = "AskHistorians", since = "2017-01-01")


library(httr)
response <- GET("https://api.pushshift.io/reddit/search/comment/?q=apple")

parsed <- jsonlite::fromJSON(
  httr::content(
    response,
    type = "text",
    encoding = "UTF-8"
  ),
  simplifyVector = FALSE
)

View(parsed$data)

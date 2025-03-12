rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(tictoc)
library(stringr)
library(lubridate)
library(sentimentr)
library(tidyr)

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

tic()
article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-11-01",
                                          end_date = "2024-11-05")
toc()

# step 2: scrape articles and return a dataframe
results <- sa_scrape_articles(article_urls, verbose = TRUE)

# step 3: clean dataframe
words_to_change <- c(" and" = ",")
results2 <- ss_clean_author(results, words_to_change = words_to_change)

results3 <- ss_clean_date(results2) %>%
  na.omit()
View(results3)

# store articles
ss_store_articles(
  article_data = results3,
  news_site = "huffpost",
  overwrite = TRUE
)

# step 4: sentiment analysis
# pull articles if you need to
test_df <- ss_pull_articles(start_date = "2024-11-01",
                            end_date = "2024-11-05",
                            news_site = "huffpost")
View(test_df)

# get sentiment of articles and sd on article level
sentiment_df <- as_article_sentiment(test_df)
View(sentiment_df)

# step 5: sentiment analysis data storage
ss_store_articles(
  article_data = sentiment_df,
  news_site = "huffpost",
  overwrite = TRUE
  # TODO fix overwrite needing to be true for added sentiment columns?? maybe
)

# pull articles with new data added
test_df2 <- ss_pull_articles(start_date = "2024-03-01",
                             end_date = "2024-03-05",
                             news_site = "huffpost")
View(test_df2)

# get sentiment for author, date, or both
# df without sentiment cols
grouped_df <- as_sentiment_grouped(test_df, group_by = "both")
View(grouped_df) # dont try to store this in the csv

# df with sentiment cols
grouped_df2 <- as_sentiment_grouped(test_df2, group_by = "both")
View(grouped_df2) # dont try to store this in the csv
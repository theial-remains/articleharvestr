rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2015-01-01",
                                          end_date = "2025-01-05")
View(article_urls)

article_urls2 <- gu_remove_duplicates(article_urls) # FIXME maybe possibly does not work
View(article_urls2)

# TODO make a function check_content for any file path (of json)
# opens as csv

# step 2: scrape articles and return a dataframe
results <- sa_scrape_articles(article_urls2, verbose = TRUE)

# step 3: clean dataframe
words_to_change <- c("and" = ",")
results2 <- ss_clean_author(results, words_to_change = words_to_change)

results3 <- ss_clean_date(results2) %>%
  na.omit()
View(results3)

# store articles
ss_store_articles(
  article_data = results3,
  news_site = "huffpost",
  overwrite = FALSE
)

# step 4: sentiment analysis
# pull articles if you need to
test_df <- ss_pull_articles(start_date = "2024-05-01",
                            end_date = "2024-05-05",
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
)

# pull articles with new data added
test_df2 <- ss_pull_articles(start_date = "2024-05-01",
                             end_date = "2024-05-05",
                             news_site = "huffpost")
View(test_df2)


# extra stuff
# get sentiment for author, date, or both
# df with sentiment cols
grouped_df2 <- as_sentiment_grouped(test_df2, group_by = "both")
View(grouped_df2) # dont try to store this either
# TODO check word count filtering
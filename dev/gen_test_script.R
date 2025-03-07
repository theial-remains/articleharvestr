rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(tictoc)
library(stringr)
library(lubridate)
library(sentimentr)

# done for huffpost:
# scrape_articles done
# get_url_list done
# store scraped data done

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

tic()
article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2016-01-01",
                                          end_date = "2016-01-01")
toc()

# step 2: scrape articles and return a dataframe
tic()
results <- sa_scrape_articles(article_urls)
toc()

# step 3: clean dataframe
words_to_change <- c("and" = ",")
results2 <- ss_clean_author(results, words_to_change = words_to_change)

results3 <- ss_clean_date(results2) %>%
  na.omit()
View(results3)

# store articles
# TODO update for dev and package mode
ss_store_articles(
  article_data = results3,
  news_site = "huffpost",
  overwrite = FALSE
)

# pull random articles
test_articles <- ss_pull_random_articles(
  start_date = "2021-01-01",
  end_date = "2021-01-03",
  news_site = "huffpost",
  num_articles = 10)
View(test_articles)

# step 4: sentiment analysis

# sentiment, random error of sentiment, by author, date, both
# visualize sentiment over time




# step 5: sentiment analysis data storage seperately
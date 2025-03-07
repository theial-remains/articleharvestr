rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(tictoc)
library(stringr)
library(lubridate)

# done for huffpost:
# scrape_articles done
# get_url_list done
# store scraped data done

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2021-01-01",
                                          end_date = "2021-01-01")
# 264.06 sec elapsed for "2020-01-01" to "2020-02-01", 1mo

# step 2: scrape articles and return a dataframe
tic()
results <- sa_scrape_articles(article_urls)
toc()

results_nona <- results %>%
  filter(!(is.na(author) & is.na(published_date) & is.na(text) & is.na(title)))

sum(is.na(results_nona$author))
sum(is.na(results_nona$published_date))
sum(is.na(results_nona$text))
sum(is.na(results_nona$title))


# step 3: clean dataframe
words_to_change <- c("and" = ",")

results2 <- ss_clean_author(results, words_to_change = words_to_change)

results3 <- ss_clean_date(results2)
sum(is.na(results_nona$published_date))

# store rows in author csvs in news site folder
# TODO update for dev and package mode
ss_store_articles(
  article_data = results3,
  news_site = "huffpost"
)

# pull 100 random articles
test_articles <- ss_pull_random_articles(
  start_date = "2020-01-01",
  end_date = "2024-02-01",
  news_site = "huffpost"
)

# step 4: sentiment analysis


# step 5: sentiment analysis data storage
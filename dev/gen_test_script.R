rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

# scrape articles
# DONT NEED THIS FOR HUFFPOST ANYMORE... probably...
# ALL ARTICLE URLS /SHOULD/ BE IN inst/extdata/huffpost/index.json
# update ur dev mode packages yall
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-01-01",
                                          end_date = "2024-12-31")
View(article_urls)

article_urls2 <- gu_remove_duplicates(article_urls)

sd_store_articles(
  article_data = article_urls,
  news_site = "huffpost",
  overwrite = FALSE
)

# pull urls (or just continue from the last step)
test_data <- sd_pull_articles(
  start_date = "2025-01-01",
  end_date = "2025-02-01",
  news_site = "huffpost",
  url = TRUE
)
View(test_data)

scraped_data <- sa_scrape_articles(test_data)

# make sure to do this before you store
# if storing the data is taking 59874594 years then its probably that
# dont ask me how I know
# that would be a stupid mistake for the creator of the package to make
# yeahhhhhh definitely didnt do that
scraped_data2 <- scraped_data %>%
  sd_clean_author() %>%
  sd_clean_date
View(scraped_data2)

sd_store_articles(
  article_data = scraped_data2,
  news_site = "huffpost",
  overwrite = FALSE
) # TODO should add tictoc to this


# USE DIS PPL
# take random sample of urls, scrape them, and analyze them
# not doing random sample of scraped urls for now
# since what articles are scraped or not is pretty random
# ik its annoying to scrape every time but eh sorry

# TODO check for misformatted dates
library(jsonlite)
library(dplyr)
library(lubridate)
index_path <- "inst/extdata/article_data/huffpost/index.json"
index_df <- read_json(index_path, simplifyVector = TRUE)
View(index_df)

# identify invalid or misformatted dates
bad_dates <- index_df %>%
  mutate(valid_format = grepl("^\\d{4}-\\d{2}-\\d{2}$", published_date)) %>%
  filter(!valid_format)
View(bad_dates)

# fix bad dates
good_dates <- bad_dates %>%
  sd_clean_date()
View(good_dates)

# store good dates
sd_store_articles(
  article_data = good_dates,
  news_site = "huffpost",
  overwrite = TRUE
)

sampled_urls <- sd_sample_urls(
  start_date = "2024-01-01", # random aah date range,
  end_date = "2024-01-31", # TODO would cause issues if it was like 2023-03-01
  # aka need to idiot proof this
  # its me im the idiot
  news_site = "huffpost",
  number = 100,
  period = "month"
)
View(sampled_urls)

check <- sd_pull_articles(start_date = "2024-01-01",
                          end_date = "2024-01-31",
                          news_site = "huffpost",
                          url = TRUE)
View(check)

# sampled_urls2 <- gu_remove_duplicates(article_urls)

scraped_data_2024 <- sa_scrape_articles(sampled_urls)
View(scraped_data)

sentiment_data <- as_article_sentiment(scraped_data)
View(sentiment_data)

# now u can store random sample of sentiment data
# or analyze it or make plots, whatever u want
sd_store_articles(
  article_data = sentiment_data,
  news_site = "huffpost",
  overwrite = FALSE
)


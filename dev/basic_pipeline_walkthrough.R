rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")
install.packages("tictoc")

devtools::load_all()

# load all urls from a date range
test_data <- sd_pull_articles(
  start_date = "2025-01-01",
  end_date = "2025-02-01",
  news_site = "huffpost",
  url = TRUE
)
View(test_data)

# or sample urls from a date range
sampled_urls <- sd_sample_urls(
  start_date = "2025-01-01",
  end_date = "2025-02-01",
  news_site = "huffpost",
  number = 100,
  period = "month"
)
View(sampled_urls)

# scrape your urls
scraped_data <- sa_scrape_articles(sampled_urls)
View(scraped_data)

# clean the author names and published dates of your scraped data
scraped_data2 <- scraped_data %>%
  sd_clean_author() %>%
  sd_clean_date()
View(scraped_data2)

# get the sentiment data (value, sd) for each article
sentiment_data <- as_article_sentiment(scraped_data2)
str(sentiment_data)

# OPTIONAL
# store your data in the json files
sd_store_articles(
  article_data = sentiment_data,
  news_site = "huffpost"
)

# now you can analyze the data however you want!

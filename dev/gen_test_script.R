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
                                          start_date = "2025-01-01",
                                          end_date = "2025-02-01")
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
# HOLY FUCK THAT IS SLOW D:


# USE DIS PPL
# take random sample of urls, scrape them, and analyze them
# not doing random sample of scraped urls for now
# since what articles are scraped or not is pretty random
# ik its annoying to scrape every time but eh sorry
sampled_urls <- sd_sample_urls(
  start_date = "2023-01-01", # random aah date range,
  end_date = "2023-03-31", # TODO would cause issues if it was like 2023-03-01
  # aka need to idiot proof this
  # its me im the idiot
  news_site = "huffpost",
  number = 100,
  period = "month"
)
View(sampled_urls)

scraped_data <- sa_scrape_articles(sampled_urls)
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

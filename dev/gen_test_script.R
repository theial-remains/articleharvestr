rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(tictoc)
library(stringr)

# done for huffpost:
# scrape_articles done
# get_url_list done
# store scraped data done

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-12-01",
                                          end_date = "2024-12-01")

# step 2: scrape articles and return a dataframe
tic()
results <- sa_scrape_articles(article_urls)
toc()
View(results)

# step 3: clean dataframe
words_to_remove <- c("trends",
                     "associate",
                     "senior",
                     "reporter",
                     "huff",
                     "post",
                     "trends",
                     "editor",
                     "shopping",
                     "for",
                     "on",
                     "at",
                     "assignment")
results2 <- ss_clean_dates(results)
View(results2)


# store rows in author csvs in news site folder
ss_store_articles(
  article_data = results3,
  news_site = "huffpost",
  folder_path = "inst/extdata/article_data/"
)

# pull 100 random articles
# TODO ss_pull_random_articles function not tested yet

# step 4: sentiment analysis

# step 5: sentiment analysis data storage
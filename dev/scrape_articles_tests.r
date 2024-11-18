rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(rvest)
library(httr)
library(purrr)
library(stringr)

article <- sa_scrape_article_data("https://www.huffpost.com/entry/mafia-betray-gospel-bishops_n_6407956")
View(article)


results <- su_read_csv("https://www.huffpost.com")
View(results)

# TODO make this take the data for classes from the csv not hardcoded
# before doing that DO NOT FORGET to correct the cols in website_schemas
# DO NOT debug for an hour over this plz future Thea

# sys.sleep needs to be longer I think??? keep getting 404ed
test1 <- head(results, n = 5)
scraped_articles <- sa_scrape_articles(test1)
View(scraped_articles)

# TODO function to write data into corresponding rows
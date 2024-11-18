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

scraped_articles <- sa_scrape_articles(results[1:5])
View(scraped_articles)

# TODO function to write data into corresponding rows
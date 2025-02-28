rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(furrr)
library(rvest)
library(httr)
library(purrr)
library(stringr)

# CNN
article_html1 <- sa_get_html("https://www.cnn.com/politics/live-news/trump-gaza-news-02-06-25/index.html")

# huffpost
article_html2 <- sa_get_html("https://www.huffpost.com/entry/emperor-penguin-gus-returned-sea-australia_n_6740d07be4b078cce4af287e")

# NBC
article_html3 <- sa_get_html("https://www.nbcnews.com/news/us-news/anxiety-mounts-us-government-workers-face-buyout-deadline-rcna190987")

text <- sa_extract_text(article_html2)
text

title <- sa_extract_title(article_html2)
title

date <- sa_extract_date(article_html2)
date

author <- sa_extract_author(article_html2)
author


results1 <- sa_get_article_data("https://www.cnn.com/politics/live-news/trump-gaza-news-02-06-25/index.html")
View(results1)
View(results1$text)

results2 <- sa_get_article_data("https://www.huffpost.com/entry/emperor-penguin-gus-returned-sea-australia_n_6740d07be4b078cce4af287e")
View(results2)
View(results2$text)

results3 <- sa_get_article_data("https://www.nbcnews.com/news/us-news/anxiety-mounts-us-government-workers-face-buyout-deadline-rcna190987")
View(results3)
View(results3$text)


list <- c("https://www.huffpost.com/entry/emperor-penguin-gus-returned-sea-australia_n_6740d07be4b078cce4af287e",
           "https://www.huffpost.com/entry/federal-worker-firings-illegal_n_67c0f9cbe4b01834869f0d79")

results4 <- sa_scrape_articles(list)
View(results4)
View(results4$text)
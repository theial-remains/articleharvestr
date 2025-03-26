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

test_title <- sa_extract_title(article_html1)
test_title

test_author <- sa_extract_author(article_html1)
test_author

test_date <- sa_extract_date(article_html1)
test_date

test_text <- sa_extract_text(article_html1)
test_text

# huffpost
article_html2 <- sa_get_html("https://www.huffpost.com/entry/ellen-degeneres-20th-anniversary-wife-portia-de-rossi_n_674cd7fee4b09da02dde3f2c")

sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
selectors <- sa_get_selectors(sitemap_url)
selectors

test_author <- sa_extract_author(article_html2, selectors$author_element, selectors$author_tag)
test_author

test_title <- sa_extract_title(article_html2, selectors$title_element, selectors$title_tag)
test_title

test_date <- sa_extract_date(article_html2, selectors$date_element, selectors$date_tag)
test_date

test_text <- sa_extract_text(article_html2, selectors$text_element, selectors$text_tag)
test_text


test_article_data <- sa_get_article_data("https://www.huffpost.com/entry/emperor-penguin-gus-returned-sea-australia_n_6740d07be4b078cce4af287e",
                                         selectors)
print(test_article_data)


list <- c("https://www.huffpost.com/entry/emperor-penguin-gus-returned-sea-australia_n_6740d07be4b078cce4af287e")

results4 <- sa_scrape_articles(list)
View(results4)

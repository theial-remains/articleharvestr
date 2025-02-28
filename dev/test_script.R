rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(tictoc)

#' title
#'
#' description
#' @param params1 parameter desc
#' @return what it returns
#' @export
ex_function <- function(params1) {
  # code goes here
}

# done:
# scrape_articles done
# get_urls done


# TODO fix verbose
# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-12-01",
                                          end_date = "2024-12-07")

# step 2 take a random sample of 100 articles per month
article_urls <- sa_sample_article_urls(sitemap_url,
                                       year = 2024,
                                       month_start = 1,
                                       month_end = 2)
head(article_urls)
# TODO make a function that matches these urls to already-downloaded articles
# and gets those instead of rescraping them

# step 3: scrape articles and return a dataframe
tic()
results <- sa_scrape_articles(article_urls)
toc()

results <- results %>%
  mutate(date2 = lubridate::as_date(published_date)) %>%
  tibble()
View(results$date2)

# step 4: clean dataframe and store rows in author csvs in news site folder
# TODO:
# function to clean author names and publsihed date in store_url_list
# in C:\Users\Preet\OneDrive - Ursinus College\paid_labor\articleharvestr\inst\extdata/article_data
# in store_url_list
# make new folder for news site if does not already exist
# make new csv for author in news site if does not already exist
# for a df of results, append all rows to the correct csv based on author

# step 5: sentiment analysis
# in C:\Users\Preet\OneDrive - Ursinus College\paid_labor\articleharvestr\inst\extdata/sentiment_analysis

# step 6: sentiment analysis data storage

# step 7: integrate data and maybe code from the rest of dsi
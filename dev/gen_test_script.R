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

# done for huffpost:
# scrape_articles done
# get_url_list done
# store scraped data done

# TODO fix verbose

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-12-01",
                                          end_date = "2024-12-03")

# step 2: scrape articles and return a dataframe
tic()
results <- sa_scrape_articles(article_urls) # TODO needs to indicate if used selectors sucessfully
toc()
View(results)

# step 3: clean dataframe
words_to_remove <- c("By") # TODO needs to remove blank spaces and alltolower
results2 <- ss_clean_author(results, words_to_remove)

results3 <- ss_clean_date(results2)
View(results3)

# store rows in author csvs in news site folder


# pull 100 random articles





# step 4: sentiment analysis

# step 5: sentiment analysis data storage
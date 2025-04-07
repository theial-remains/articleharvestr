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
) # FIXME should add tictoc to this



# TODO check for misformatted dates
library(jsonlite)
library(dplyr)
library(lubridate)
index_path <- "inst/extdata/article_data/huffpost/index.json"
index_df <- read_json(index_path, simplifyVector = TRUE)
head(index_df)

# identify invalid or misformatted dates
bad_dates <- index_df %>%
  mutate(valid_format = grepl("^\\d{4}-\\d{2}-\\d{2}$", published_date)) %>%
  filter(!valid_format)
head(bad_dates)

# fix bad dates
good_dates <- bad_dates %>%
  sd_clean_date()
head(good_dates)

# store good dates
sd_store_articles(
  article_data = good_dates,
  news_site = "huffpost",
  overwrite = TRUE
) # Why so slow :(


# FIXME
# FIXME
# FIXME fuckin asap
# pretty sure new/updated rows reporting aint working correctly
# for sd_store_articles
# and it slow af gotta fix that


# hopefully works now
sampled_urls <- sd_sample_urls(
  start_date = "2017-01-01", # random aah date range,
  end_date = "2025-03-01",
  news_site = "huffpost",
  number = 100,
  period = "month"
)
View(sampled_urls)

# TODO get random sample and do monthly sentiment analysis

# check <- sd_pull_articles(start_date = "2024-01-01",
#                           end_date = "2024-01-31",
#                           news_site = "huffpost",
#                           url = TRUE)
# View(check)

scraped_data <- sa_scrape_articles(sampled_urls)
str(scraped_data)

sentiment_data <- as_article_sentiment(scraped_data)
View(sentiment_data)

sentiment_data2 <- sentiment_data %>%
  sd_clean_author() %>%
  sd_clean_date()
View(sentiment_data2)

sd_store_articles(
  article_data = sentiment_data2,
  news_site = "huffpost",
  overwrite = FALSE
)


sentiment_data_sample2 <- readRDS("random_shit_folder/all_years_sentiment_sample.RDS")



# analysis of sample
library(dplyr)
library(tidyr)
library(plotly)

author_counts <- sentiment_data_sample2 %>%
  filter(!is.na(author), !is.na(sentiment_val)) %>%
  group_by(author, period) %>%
  tally(name = "n_articles")

complete_authors <- author_counts %>%
  filter(n_articles >= 10) %>%
  count(author) %>%
  filter(n == 3) %>%
  pull(author)

author_period_avg <- sentiment_data_sample2 %>%
  filter(author %in% complete_authors) %>%
  group_by(author, period) %>%
  summarise(mean_sentiment = mean(sentiment_val, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = factor(period, levels = c("Before COVID", "During COVID", "After COVID")))

p <- plot_ly(type = 'violin')

for (p_label in levels(author_period_avg$period)) {
  p <- add_trace(p,
    data = filter(author_period_avg, period == p_label),
    y = ~mean_sentiment,
    name = p_label,
    type = 'violin',
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    points = 'all',
    jitter = 0.3,
    opacity = 0.5,
    showlegend = TRUE
  )
}

# line data connecting each author
lines_data <- author_period_avg %>%
  tidyr::pivot_wider(names_from = period, values_from = mean_sentiment) %>%
  na.omit()

for (i in 1:nrow(lines_data)) {
  p <- add_trace(p,
    x = c("Before COVID", "During COVID", "After COVID"),
    y = as.numeric(lines_data[i, 2:4]),
    type = 'scatter',
    mode = 'lines+markers',
    name = lines_data$author[i],
    line = list(width = 1),
    marker = list(size = 4),
    opacity = 0.4,
    showlegend = FALSE
  )
}

# layout
p <- layout(p,
  title = paste("Average Sentiment by Author Across COVID Periods (Min", min_n_per_period, "Articles/Period)"),
  yaxis = list(title = "Mean Sentiment"),
  xaxis = list(title = "COVID Period")
)

p

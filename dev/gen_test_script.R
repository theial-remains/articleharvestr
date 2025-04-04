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



# analysis of sample
library(dplyr)
library(ggplot2)
library(lubridate)

# removed rows with NA values or less than 25 words in text
sentiment_data_sample <- readRDS("random_shit_folder/all_years_sentiment_sample.RDS")
View(sentiment_data_sample)

# distribution of overall sentiment
ggplot(sentiment_data_sample, aes(x = sentiment_val)) +
  geom_density(fill = "darkgreen", alpha = 0.4) +
  labs(
    title = "Distribution of Sentiment Scores",
    x = "Sentiment Score",
    y = "Density"
  ) +
  theme_minimal()

# average sentiment by month
monthly_sentiment <- sentiment_data_sample %>%
  mutate(month = floor_date(as.Date(published_date), "month")) %>%
  group_by(month) %>%
  summarise(
    avg_sentiment = mean(sentiment_val, na.rm = TRUE),
    sd_sentiment = sd(sentiment_val, na.rm = TRUE),
    .groups = "drop"
  )


# TODO seperate geom_smooth for pre and post covid
ggplot(monthly_sentiment, aes(x = month, y = avg_sentiment)) +
  geom_line(color = "darkblue") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = avg_sentiment - sd_sentiment,
                  ymax = avg_sentiment + sd_sentiment),
              fill = "steelblue", alpha = 0.2) +
  geom_smooth() +
  scale_x_date(date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(title = "Average Sentiment by Month",
       x = "Month",
       y = "Average Sentiment (AFINN)")

# violin plot
# TODO plotly
sentiment_data <- sentiment_data_sample %>%
  mutate(period = case_when(
    as.Date(published_date) < as.Date("2020-03-01") ~ "Before COVID",
    as.Date(published_date) >= as.Date("2020-03-01") & as.Date(published_date) < as.Date("2021-06-01") ~ "During COVID",
    as.Date(published_date) >= as.Date("2021-06-01") ~ "After COVID"
  ))

sentiment_data$period <- factor(sentiment_data$period, levels = c("Before COVID", "During COVID", "After COVID"))

ggplot(sentiment_data, aes(x = period, y = sentiment_val, fill = period)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  labs(title = "Sentiment Distribution Across COVID Periods",
       x = "Period",
       y = "Sentiment Score") +
  theme_minimal()

# test for difference in sentiment
# Mann-Whitney U test (non-parametric)
wilcox.test(
  sentiment_val ~ period,
  data = sentiment_data
)
# sentiment didnt stay low?? maybe?

# TODO by author
unique(sentiment_data$author)
author_sentiment <-

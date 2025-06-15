
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
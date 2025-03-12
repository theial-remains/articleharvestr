#' Perform Sentiment Analysis on Articles (Optimized & Fixed)
#'
#' @param dataframe A data frame with a "text" column containing article text.
#' @return The same data frame with additional columns: "sentiment_val" and "sentiment_sd".
#' @import sentimentr
#' @import dplyr
#' @export
as_article_sentiment <- function(dataframe) {
  if (!"text" %in% names(dataframe)) {
    stop("Error: The input data frame must contain a 'text' column.")
  }

  # add row index
  dataframe <- dataframe %>%
    mutate(row_id = row_number(), word_count = str_count(text, "\\S+"))

  # rm empty or short texts
  valid_data <- dataframe %>% filter(!is.na(text) & word_count >= 5)

  if (nrow(valid_data) == 0) {
    dataframe$sentiment_val <- NA
    dataframe$sentiment_sd <- NA
    return(dataframe)
  }

  # do sentiment analysis
  sentiment_results <- sentiment_by(get_sentences(valid_data$text), valid_data$row_id)

  dataframe <- dataframe %>%
    left_join(sentiment_results %>% select(row_id, ave_sentiment, sd), by = "row_id") %>%
    mutate(sentiment_val = ifelse(is.na(ave_sentiment), NA, ave_sentiment),
           sentiment_sd = ifelse(is.na(sd), NA, sd)) %>%
    select(-row_id, -word_count, -ave_sentiment, -sd) %>%
    mutate(published_date = as.character(published_date))

  return(dataframe)
}

#' Perform Sentiment Analysis Grouped by Author, Date, or Both
#'
#' Computes sentiment for articles if not already done, then calculates
#' sentiment averages grouped by author, date, or both.
#'
#' @param dataframe A data frame containing article text, author, and published_date.
#' @param group_by Character: "author", "date", or "both" (default: "both").
#' @return A data frame with sentiment scores grouped as specified.
#' @import sentimentr
#' @import dplyr
#' @import lubridate
#' @export
as_sentiment_grouped <- function(dataframe, group_by = "both") {
  if (!all(c("text", "author", "published_date") %in% names(dataframe))) {
    stop("Error: Dataframe must contain 'text', 'author', and 'published_date' columns.")
  }

  group_by <- tolower(group_by)

  # if sentiment values are not there yet
  if (!"sentiment_val" %in% names(dataframe) || all(is.na(dataframe$sentiment_val))) {
    message("Computing sentiment scores for articles...")
    dataframe <- as_article_sentiment(dataframe)
  }

  if (!all(c("text", "author", "published_date", "sentiment_val", "sentiment_sd") %in% names(dataframe))) {
    stop("Error: Incorrect column names.")
  }

  if (group_by == "author") {
    sentiment <- dataframe %>%
      group_by(author) %>%
      summarise(
        mean_sentiment = mean(sentiment_val, na.rm = TRUE),
        sd_sentiment = mean(sentiment_sd, na.rm = TRUE),
        total_articles = n()
      )
  } else if (group_by == "date") {
    sentiment <- dataframe %>%
      group_by(published_date) %>%
      summarise(
        mean_sentiment = mean(sentiment_val, na.rm = TRUE),
        sd_sentiment = mean(sentiment_sd, na.rm = TRUE),
        total_articles = n()
      )
  } else if (group_by == "both") {
    sentiment <- dataframe %>%
      group_by(author, published_date) %>%
      summarise(
        mean_sentiment = mean(sentiment_val, na.rm = TRUE),
        sd_sentiment = mean(sentiment_sd, na.rm = TRUE),
        total_articles = n()
      )
  } else {
    stop("Error: 'group_by' must be 'author', 'date', or 'both'.")
  }

  return(sentiment)
}

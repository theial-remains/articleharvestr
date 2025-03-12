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

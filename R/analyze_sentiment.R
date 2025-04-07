#' Perform Sentiment Analysis on Articles (Optimized & Fixed)
#'
#' @param dataframe A data frame with a "text" column containing article text.
#' @return The same data frame with additional columns: "sentiment_val" and "sentiment_sd".
#' @import sentimentr
#' @import dplyr
#' @import tidyr
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
    left_join(sentiment_results %>%
    select(row_id, ave_sentiment, sd), by = "row_id") %>%
    mutate(sentiment_val = ifelse(is.na(ave_sentiment), NA, ave_sentiment),
           sentiment_sd = ifelse(is.na(sd), NA, sd)) %>%
    select(-row_id, -word_count, -ave_sentiment, -sd) %>%
    mutate(published_date = as.character(published_date))

  return(tibble(dataframe))
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
      mutate(sentences = get_sentences(text)) %>%
      with(sentiment_by(sentences, list(author)))
  } else if (group_by == "date") {
    sentiment <- dataframe %>%
      mutate(sentences = get_sentences(text)) %>%
      with(sentiment_by(sentences, list(published_date)))
  } else if (group_by == "both") {
    sentiment <- dataframe %>%
      mutate(sentences = get_sentences(text)) %>%
      with(sentiment_by(sentences, list(published_date, author)))
  } else {
    stop("Error: 'group_by' must be 'author', 'date', or 'both'.")
  }

  sentiment2 <- sentiment %>%
    mutate(sd_avg = sd / sqrt(word_count))

  return(sentiment2)
}

#' Replace Articles with Missing or Incomplete Data
#'
#' Identifies articles with missing values or too few words and returns
#' replacement URLs from the same time periods using `sd_sample_urls()`.
#'
#' @param dataframe A data frame with columns: url, published_date, author, text.
#' @param news_site Name of the news site (used to find the correct index.json).
#' @param period Time period for sampling ("day", "month", or "year").
#' @param ran_number Number of articles that should exist per period.
#' @param min_words Minimum number of words required in the article text.
#'
#' @return A tibble of replacement URLs with their associated published_date.
#' @export
sd_replace_bad_articles <- function(dataframe,
                                    news_site,
                                    period = c("day", "month", "year"),
                                    ran_number = 100,
                                    min_words = 5) {
  period <- match.arg(period)

  # make sure published_date is Date
  dataframe$published_date <- as.Date(dataframe$published_date)

  # create grouping key
  dataframe$group <- dplyr::case_when(
    period == "day" ~ format(dataframe$published_date, "%Y-%m-%d"),
    period == "month" ~ format(dataframe$published_date, "%Y-%m"),
    period == "year" ~ format(dataframe$published_date, "%Y")
  )

  # count expected articles per group
  expected_counts <- dataframe %>%
    dplyr::group_by(group) %>%
    dplyr::tally(name = "expected")

  # remove articles with missing key values or short text
  dataframe$word_count <- stringr::str_count(dataframe$text, "\\S+")

  bad_articles <- dataframe %>%
    dplyr::filter(
      is.na(author) | is.na(published_date) | is.na(text) | word_count < min_words
    )

  good_data <- dplyr::anti_join(dataframe, bad_articles, by = "url")

  # count remaining articles per group
  actual_counts <- good_data %>%
    dplyr::group_by(group) %>%
    dplyr::tally(name = "actual")

  # join and find how many are missing
  counts <- dplyr::left_join(expected_counts, actual_counts, by = "group") %>%
    dplyr::mutate(
      actual = tidyr::replace_na(actual, 0),
      need = expected - actual
    ) %>%
    dplyr::filter(need > 0)

  # report what was removed
  message("Removed ", nrow(bad_articles), " bad articles:")

  # report what periods need replacements
  message("Need replacements in ", nrow(counts), " ", period, "(s):")
  print(counts %>% dplyr::select(group, need))

  # if nothing to replace, exit early
  if (nrow(counts) == 0) {
    return(tibble::tibble())
  }

  # sample replacements for each affected period
  replacements <- purrr::map_dfr(seq_len(nrow(counts)), function(i) {
    row <- counts[i, ]
    group_val <- row$group
    need_n <- row$need

    if (period == "day") {
      start_date <- end_date <- as.Date(group_val)
    } else if (period == "month") {
      start_date <- as.Date(paste0(group_val, "-01"))
      end_date <- lubridate::ceiling_date(start_date, "month") - 1
    } else if (period == "year") {
      start_date <- as.Date(paste0(group_val, "-01-01"))
      end_date <- as.Date(paste0(group_val, "-12-31"))
    }

    sd_sample_urls(
      start_date = start_date,
      end_date = end_date,
      news_site = news_site,
      number = need_n,
      period = period
    )
  })

  return(replacements)
}

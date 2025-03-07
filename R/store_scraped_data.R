#' Clean Published Dates in Dataframe
#'
#' Detects the format of each date and converts it to "YYYY-MM-DD".
#'
#' @param dataframe A dataframe containing a "published_date" column.
#' @return The same dataframe with additional columns: "guessed_format" and "cleaned_date".
#' @import lubridate
#' @export
ss_clean_date <- function(dataframe) {
  if (!"published_date" %in% names(dataframe)) {
    stop("Error: Dataframe must contain a 'published_date' column.")
  }

  dataframe$published_date <- as.character(dataframe$published_date)

  results <- t(sapply(dataframe$published_date, function(date_string) {
    if (is.na(date_string) || date_string == "") return(c(NA, NA))

    # rm time zones
    clean_date_string <- gsub("( EST| PST| CST| UTC| GMT| Z)$", "", date_string)

    # guess possible formats
    possible_formats <- guess_formats(clean_date_string, orders = c(
      "ymd", "mdy", "dmy", "Ymd HMS", "mdy HMS", "dmy HMS",
      "Y-m-d H:M:S", "b d Y", "b d, Y H:M p", "b d, Y"
    ))

    if (length(possible_formats) == 0) return(c("No format detected", NA))

    format_string <- paste(possible_formats, collapse = "; ")

    # parse the date with the first guessed format
    parsed_date <- tryCatch({
      as.Date(parse_date_time(clean_date_string, orders = possible_formats))
    }, error = function(e) {
      return(NA)
    })

    return(c(format_string, as.character(parsed_date)))
  }))

  dataframe$guessed_format <- results[, 1]
  dataframe$cleaned_date <- results[, 2]

  return(dataframe)
}

#' Clean Author Names in Dataframe
#'
#' Splits words if they are incorrectly combined (e.g., "AliceJohnson" → "Alice Johnson").
#' Converts all names to lowercase.
#' Uses str_replace_all to remove unwanted words and replace specific words.
#'
#' @param dataframe A dataframe containing an "author" column.
#' @param words_to_remove A character vector of words to remove from author names (case-insensitive).
#' @param words_to_change A named vector where keys are words to find and values are replacements.
#' @return The same dataframe with a cleaned "author" column.
#' @import stringr
#' @export
ss_clean_author <- function(dataframe,
                            words_to_remove = c(),
                            words_to_change = c()) {
  if (!"author" %in% names(dataframe)) {
    stop("Error: Dataframe must contain an 'author' column.")
  }

  dataframe$author <- as.character(dataframe$author)

  # split joined words
  dataframe$author <- gsub("([a-z])([A-Z])", "\\1 \\2",
                           dataframe$author,
                           perl = TRUE)

  # lowercase
  dataframe$author <- tolower(dataframe$author)

  # rm and replace specified words with str_replace_all
  if (length(words_to_remove) > 0) {
  words_to_remove <- setNames(rep("", length(words_to_remove)), words_to_remove)
  dataframe$author <- str_replace_all(dataframe$author, words_to_remove)
  }

  if (length(words_to_change) > 0) {
    dataframe$author <- str_replace_all(dataframe$author, regex(names(words_to_change), ignore_case = TRUE), words_to_change)
  }

  # trim leading and trailing spaces
  dataframe$author <- trimws(dataframe$author)

  return(dataframe)
}

#' Store and Append Articles for Any News Site
#'
#' This function creates the CSV if it doesn't exist and appends new articles to it,
#' ensuring no duplicate URLs are added.
#'
#' @param article_data A data frame with columns: url, title, author, published_date, text.
#' @param news_site The name of the news site (e.g., "huffpost").
#' @param folder_path Directory for storing CSV files.
#' @return The full path of the updated CSV.
#' @import dplyr
#' @export
ss_store_articles <- function(article_data,
                              news_site,
                              folder_path = "inst/extdata/article_data/") {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }

  required_columns <- c("url", "title", "author", "published_date", "text")
  if (!all(required_columns %in% names(article_data))) {
    stop("Input data frame must contain: ", paste(required_columns, collapse = ", "))
  }

  file_path <- file.path(folder_path, paste0(news_site, ".csv"))

  article_data <- ss_clean_author(article_data, c("By", "Byline"))
  article_data <- ss_clean_date(article_data)

  if (file.exists(file_path)) {
    existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
    new_articles <- subset(article_data, !url %in% existing_data$url)

    if (nrow(new_articles) == 0) {
      message("No new articles to add.")
      return(file_path)
    }

    combined_data <- rbind(existing_data, new_articles)
  } else {
    combined_data <- article_data
  }

  write.csv(combined_data, file_path, row.names = FALSE)
  message(nrow(article_data), " articles checked, ", nrow(combined_data) - nrow(existing_data), " new articles added.")
  return(file_path)
}

#' Pull Random Sample of 100 Articles from a News Site
#'
#' Retrieves a random sample of articles from a CSV for a given date range.
#'
#' @param start_date The start date (YYYY-MM-DD).
#' @param end_date The end date (YYYY-MM-DD).
#' @param news_site The name of the news site (e.g., "huffpost").
#' @param folder_path Directory where the CSV is stored.
#' @return A data frame containing the sampled articles or a message if no articles exist.
#' @import dplyr
#' @export
ss_pull_random_articles <- function(start_date,
                                    end_date,
                                    news_site,
                                    folder_path = "inst/extdata/article_data/") {
  file_path <- file.path(folder_path, paste0(news_site, ".csv"))

  if (!file.exists(file_path)) {
    stop("No article data found for ", news_site, ". Please store data first.")
  }

  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data$published_date <- as.Date(data$published_date)

  filtered_data <- subset(data, published_date >= as.Date(start_date) & published_date <= as.Date(end_date))

  if (nrow(filtered_data) == 0) {
    return("No articles found in the given date range.")
  }

  sampled_data <- filtered_data[sample(nrow(filtered_data), min(100, nrow(filtered_data))), ]

  return(sampled_data)
}
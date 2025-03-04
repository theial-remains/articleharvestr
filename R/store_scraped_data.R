#' Clean Author Names in Dataframe
#'
#' Removes specified words (case-insensitive) from author names and trims whitespace.
#' Splits words if they are incorrectly combined (e.g., "VeraSenior" → "Vera Senior").
#' Keeps only the first two words of the author's name.
#'
#' @param dataframe A dataframe containing an "author" column.
#' @param words_to_remove A character vector of words/phrases to remove from author names (case-insensitive).
#' @return The same dataframe with a cleaned "author" column.
#' @export
ss_clean_author <- function(dataframe, words_to_remove = c("By")) {
  if (!"author" %in% names(dataframe)) {
    stop("Error: Dataframe must contain an 'author' column.")
  }

  dataframe$author <- as.character(dataframe$author)

  pattern <- paste0("\\b(", paste(words_to_remove, collapse = "|"), ")\\b\\s*", collapse = "|")

  dataframe$author <- ifelse(
    is.na(dataframe$author) | dataframe$author == "",
    NA,
    trimws(gsub(pattern, "", dataframe$author, ignore.case = TRUE, perl = TRUE))
  )

  dataframe$author <- gsub("([a-z])([A-Z])",
                           "\\1 \\2",
                           dataframe$author, perl = TRUE)

  dataframe$author <- sapply(dataframe$author, function(name) {
    if (!is.na(name) && name != "") {
      words <- unlist(strsplit(name, "\\s+"))
      paste(head(words, 2), collapse = " ")
    } else {
      NA
    }
  }, USE.NAMES = FALSE)

  return(dataframe)
}

#' Clean Published Dates in Dataframe
#'
#' Extracts only the date from mixed datetime formats and converts it into "YYYY-MM-DD".
#'
#' @param dataframe A dataframe containing a "published_date" column.
#' @return The same dataframe with a cleaned "published_date" column.
#' @import lubridate
#' @export
ss_clean_date <- function(dataframe) {
  if (!"published_date" %in% names(dataframe)) {
    stop("Error: Dataframe must contain a 'published_date' column.")
  }

  dataframe$published_date <- sapply(dataframe$published_date, function(date_string) {
    if (is.na(date_string) || date_string == "") return(NA)

    tryCatch({
      date_only <- gsub(",?\\s*\\d{1,2}:\\d{2}\\s*(AM|PM)?\\s*[A-Z]*", "", date_string)

      date_only <- gsub("[^A-Za-z0-9, ]", "", date_only)

      parsed_date <- suppressWarnings(parse_date_time(
        date_only,
        orders = c("b d, Y", "b d Y", "mdy", "dmy", "ymd", "mdY")
      ))

      if (is.na(parsed_date)) return(NA)

      as.character(as.Date(parsed_date))
    }, error = function(e) {
      return(NA)
    })
  })

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
  # Generate CSV filename
  file_path <- file.path(folder_path, paste0(news_site, ".csv"))

  if (!file.exists(file_path)) {
    stop("No article data found for ", news_site, ". Please store data first.")
  }

  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data$published_date <- as.Date(data$published_date)

  # Filter articles by date range
  filtered_data <- subset(data, published_date >= as.Date(start_date) & published_date <= as.Date(end_date))

  if (nrow(filtered_data) == 0) {
    return("No articles found in the given date range.")
  }

  # Random sample of 100 articles or all available if fewer
  sampled_data <- filtered_data[sample(nrow(filtered_data), min(100, nrow(filtered_data))), ]

  return(sampled_data)
}




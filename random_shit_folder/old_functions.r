#' Store and Append Articles for Any News Site
#'
#' This function creates the CSV if it doesn't exist and appends new articles to it,
#' ensuring no duplicate URLs are added. If `overwrite = TRUE`, it replaces articles
#' with matching URLs instead of adding duplicates.
#'
#' @param article_data A data frame with columns: url, title, author, published_date, text.
#' @param news_site The name of the news site (e.g., "huffpost").
#' @param folder_path Directory for storing CSV files. Defaults to correct path based on mode.
#' @param overwrite TRUE or FALSE, option to overwrite existing articles with the same URL.
#' @return The full path of the updated CSV.
#' @import dplyr
#' @export
ss_store_articles <- function(article_data,
                              news_site,
                              folder_path = NULL,
                              overwrite = FALSE) {
  if (is.null(folder_path)) {
    dev_mode <- !nzchar(system.file(package = "articleharvestr"))
    folder_path <- if (dev_mode) {
      "inst/extdata/article_data/"
    } else {
      system.file("extdata", "article_data", package = "articleharvestr")
    }
  }

  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }

  required_columns <- c("url", "title", "author", "published_date", "text")
  if (!all(required_columns %in% names(article_data))) {
    stop("Input data frame must contain: ", paste(required_columns, collapse = ", "))
  }

  # Ensure sentiment_val and sentiment_sd exist in input data
  if (!"sentiment_val" %in% names(article_data)) {
    article_data$sentiment_val <- NA
  }
  if (!"sentiment_sd" %in% names(article_data)) {
    article_data$sentiment_sd <- NA
  }

  file_path <- file.path(folder_path, paste0(news_site, ".csv"))

  if (file.exists(file_path)) {
    existing_data <- read.csv(file_path, stringsAsFactors = FALSE)

    # see if existing data has sentiment_val and sentiment_sd
    if (!"sentiment_val" %in% names(existing_data)) {
      existing_data$sentiment_val <- NA
    }
    if (!"sentiment_sd" %in% names(existing_data)) {
      existing_data$sentiment_sd <- NA
    }

    # Ensure column names are consistent before merging
    all_columns <- c("url", "title", "author", "published_date", "text", "sentiment_val", "sentiment_sd")
    existing_data <- existing_data[, intersect(names(existing_data), all_columns), drop = FALSE]
    article_data <- article_data[, intersect(names(article_data), all_columns), drop = FALSE]

    if (nrow(existing_data) == 0) {
      combined_data <- article_data
      message("Existing CSV was empty. Writing all articles.")
    } else {
      if (overwrite) {
        existing_data <- existing_data[!(existing_data$url %in% article_data$url), ]
        message("Overwrite is TRUE. Replacing articles with matching URLs.")
      }

      new_articles <- subset(article_data, !article_data$url %in% existing_data$url)

      if (nrow(new_articles) == 0 && !overwrite) {
        message("No new articles to add.")
        return(file_path)
      }

      combined_data <- bind_rows(existing_data, new_articles)
    }
  } else {
    combined_data <- article_data
    message("No existing CSV found. Creating CSV. Writing all articles.")
  }

  # Ensure final column order before saving
  final_columns <- c("url", "title", "author", "published_date", "text", "sentiment_val", "sentiment_sd")
  combined_data <- combined_data[, final_columns, drop = FALSE]

  write.csv(combined_data, file_path, row.names = FALSE)
  message(nrow(combined_data) - ifelse(exists("existing_data") && !is.null(existing_data), nrow(existing_data), 0),
          " articles added or updated.")

  return(file_path)
}

#' Pull Articles from a News Site
#'
#' Retrieves articles from a CSV for a given date range.
#'
#' @param start_date The start date in YYYY-MM-DD format.
#' @param end_date The end date in YYYY-MM-DD format.
#' @param ran_articles The number of random articles to sample. If NULL, returns all articles (default: NULL).
#' @param news_site The name of the news site (e.g., "huffpost").
#' @return A data frame containing the selected articles or an error message if conditions are not met.
#' @import dplyr
#' @import lubridate
#' @export
ss_pull_articles <- function(start_date,
                             end_date,
                             ran_articles = NULL,
                             news_site) {
  dev_mode <- !nzchar(system.file(package = "articleharvestr"))
  folder_path <- if (dev_mode) "inst/extdata/article_data/" else system.file("extdata", "article_data", package = "articleharvestr")

  file_path <- file.path(folder_path, paste0(news_site, ".csv"))

  if (!file.exists(file_path)) {
    stop("Error: No article data found for ", news_site, ". Please store data first.")
  }

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # message("Checking `published_date` column BEFORE conversion:")
  # print(head(data$published_date)) # error checking

  # message("Checking data structure:")
  # print(str(data$published_date)) # error checking

  if (!inherits(data$published_date, "Date")) {
    date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y")
    for (fmt in date_formats) {
      converted_dates <- as.Date(data$published_date, format = fmt)
      if (sum(!is.na(converted_dates)) > 0) {
        data$published_date <- converted_dates
        message("Successfully converted `published_date` using format: ", fmt)
        break
      }
    }
  }

  # message("`published_date` column after conversion:")
  # print(head(data$published_date))

  if (nrow(data) == 0) {
    stop("Error: The dataset is empty.")
  }

  available_dates <- sort(unique(na.omit(data$published_date)))

  if (length(available_dates) == 0) {
    stop("Error: No valid dates found in the dataset.")
  }

  breaks <- c(1, diff(available_dates) > 1)
  range_indices <- cumsum(breaks)
  available_ranges <- split(available_dates, range_indices)
  formatted_ranges <- sapply(available_ranges, function(x) {
    if (length(x) == 1) return(as.character(x))
    return(paste(min(x), "to", max(x)))
  })

  # message("Available date ranges: ", paste(formatted_ranges, collapse = ", "))

  requested_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  covered_dates <- unlist(available_ranges)

  covered_by_range <- any(sapply(available_ranges, function(range) all(requested_dates %in% range)))

  if (!covered_by_range) {
    stop(paste0("Error: The requested date range is outside available data. Available ranges: ", paste(formatted_ranges, collapse = ", ")))
  }

  # message("Filtering data for requested date range...")
  filtered_data <- subset(data, published_date >= as.Date(start_date) & published_date <= as.Date(end_date))

  # message("Number of rows after filtering: ", nrow(filtered_data))

  if (nrow(filtered_data) == 0) {
    stop("Error: No articles found in the given date range.")
  }

  if (!is.null(ran_articles)) {
    if (nrow(filtered_data) < ran_articles) {
      stop(paste0("Error: Only ", nrow(filtered_data), " articles available. Requested ", ran_articles, "."))
    }
    filtered_data <- filtered_data[sample(nrow(filtered_data), ran_articles), ]
  }

  return(filtered_data)
}

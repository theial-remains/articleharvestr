#' Clean Published Dates in Dataframe
#'
#' Detects the format of each date and converts it to "YYYY-MM-DD".
#'
#' @param dataframe A dataframe containing a "published_date" column.
#' @return The same dataframe with "published_date" cleaned to "YYYY-MM-DD".
#' @import lubridate
#' @export
ss_clean_date <- function(dataframe) {
  if (!"published_date" %in% names(dataframe)) {
    stop("Error: Dataframe must contain a 'published_date' column.")
  }

  dataframe$published_date <- as.character(dataframe$published_date)

  dataframe$published_date <- sapply(dataframe$published_date, function(date_string) {
    if (is.na(date_string) || date_string == "") return(NA)

    # do ISO 8601 manually because I cannot be bothered
    if (grepl("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", date_string)) {
      return(as.character(as.Date(ymd_hms(date_string))))
    }

    # rm time zones
    clean_date_string <- gsub("( EST| EDT| PDT| CDT| PST| CST| UTC| GMT| Z)$", "", date_string)

    # guess formats
    possible_formats <- guess_formats(clean_date_string, orders = c(
      "ymd", "mdy", "dmy",
      "Ymd HMS", "mdy HMS", "dmy HMS",
      "Y-m-d H:M:S", "b d Y", "b d, Y H:M p", "b d, Y",
      "b d, Y H:M p", "b d, Y H:M", "b d, Y H:M:S p"
    ))

    if (length(possible_formats) == 0) return(clean_date_string)

    # use the first guessed format for date
    parsed_date <- tryCatch({
      as.Date(parse_date_time2(clean_date_string,
                               orders = possible_formats[1]))
    }, error = function(e) {
      return(NA)
    })

    return(as.character(parsed_date))
  })

  dataframe$published_date <- as.character(dataframe$published_date)

  dataframe <- dataframe %>%
    select(url, published_date, author, title, text)

  return(dataframe)
}

#' Clean Author Names in Dataframe
#'
#' Splits words if they are incorrectly combined (e.g., "AliceJohnson" → "Alice Johnson").
#' Converts all names to lowercase.
#' Removes unwanted words and replaces specific words while ensuring whole-word matching.
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

  dataframe$author <- tolower(dataframe$author)

  # rm specified words, whole word match only
  if (length(words_to_remove) > 0) {
    pattern_remove <- paste0("\\b(", paste(words_to_remove, collapse = "|"), ")\\b")
    dataframe$author <- str_replace_all(dataframe$author, regex(pattern_remove, ignore_case = TRUE), "")
  }

  # repl specified words, whole word match only
  if (length(words_to_change) > 0) {
    pattern_replace <- paste0("\\b(", paste(names(words_to_change), collapse = "|"), ")\\b")
    dataframe$author <- str_replace_all(dataframe$author, regex(pattern_replace, ignore_case = TRUE), function(x) words_to_change[tolower(x)])
  }

  # rm extra spaces
  dataframe$author <- str_squish(dataframe$author)

  return(dataframe)
}

#' Store articles using decorator pipeline
#'
#' This function stores metadata in index.json and full data in monthly jsons if text is present.
#'
#' @param article_data A data frame with at least url and published_date columns.
#' @param news_site The name of the news site (e.g., "huffpost").
#' @param overwrite If TRUE, allows replacing all fields for matching URLs. If FALSE, only updates missing fields.
#' @return NULL (invisible)
#' @export
ss_store_articles <- function(article_data, news_site, overwrite = FALSE) {
  if (!"url" %in% names(article_data) || !"published_date" %in% names(article_data)) {
    stop("article_data must contain at least 'url' and 'published_date'")
  }

  article_data$published_date <- as.character(article_data$published_date)

  # always runs:
  # creates site folder if needed
  # adds data in index.json for that site (or not if overwrite FALSE and data not new)
  index_pipeline <- compose_storage(
    store_ensure_folders,
    store_index_json,
    store_base()
  )
  index_pipeline(article_data, news_site, overwrite)

  # store full articles if text is present
  # creates site folder if needed
  # create or update monthly jsons (or not if overwrite FALSE and data not new)
  if ("text" %in% names(article_data)) {
    text_data <- article_data[!is.na(article_data$text) & article_data$text != "", ]

    if (nrow(text_data) > 0) {
      monthly_pipeline <- compose_storage(
        store_ensure_folders,
        store_index_json,
        store_monthly_json,
        store_base()
      )
      monthly_pipeline(text_data, news_site, overwrite)
    }
  }

  invisible(NULL)
}

#' Pull articles from json files or index metadata
#'
#' Retrieves article metadata and/or full data for a given site and date range.
#'
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param news_site Name of the news site (e.g., "huffpost")
#' @param ran_articles Optional number of random articles to sample
#' @param needs_text If TRUE/FALSE, filter for presence/absence of article text (default: NULL = return all)
#' @param needs_sentiment If TRUE/FALSE, filter for presence/absence of sentiment values (default: NULL = return all)
#' @param from_index If TRUE, loads from index.json instead of monthly files (default: FALSE)
#' @return A tibble of article data
#' @export
ss_pull_articles <- function(start_date,
                             end_date,
                             news_site,
                             ran_articles = NULL,
                             needs_text = NULL,
                             needs_sentiment = NULL,
                             from_index = FALSE) {
  folder <- file.path("inst/extdata/article_data", news_site)

  if (from_index) {
    # load index only
    index_path <- file.path(folder, "index.json")
    if (!file.exists(index_path)) stop("index.json does not exist for site: ", news_site)

    articles <- jsonlite::read_json(index_path, simplifyVector = TRUE)
    articles <- dplyr::filter(articles,
      as.Date(published_date) >= as.Date(start_date),
      as.Date(published_date) <= as.Date(end_date))

  } else {
    # load monthly full text data
    all_months <- list.files(folder, pattern = "\\d{4}-\\d{2}\\.json$", full.names = TRUE)
    file_dates <- as.Date(sub(".json$", "-01", basename(all_months)))
    date_range <- as.Date(c(start_date, end_date))
    keep_files <- all_months[file_dates >= date_range[1] & file_dates <= date_range[2]]

    articles <- purrr::map_dfr(keep_files, ~ jsonlite::read_json(.x, simplifyVector = TRUE))
    articles <- dplyr::filter(articles,
      as.Date(published_date) >= as.Date(start_date),
      as.Date(published_date) <= as.Date(end_date))
  }

  # filter by presence/absence of text
  if (!is.null(needs_text)) {
    if (needs_text) {
      articles <- dplyr::filter(articles, !is.na(text) & text != "")
    } else {
      articles <- dplyr::filter(articles, is.na(text) | text == "")
    }
  }

  # filter by presence/absence of sentiment
  if (!is.null(needs_sentiment)) {
    if (needs_sentiment) {
      articles <- dplyr::filter(articles, !is.na(sentiment_val))
    } else {
      articles <- dplyr::filter(articles, is.na(sentiment_val))
    }
  }

  # optionally sample
  if (!is.null(ran_articles)) {
    if (nrow(articles) < ran_articles) {
      warning("Requested ", ran_articles, " but only ", nrow(articles), " articles available.")
    } else {
      articles <- articles[sample(nrow(articles), ran_articles), ]
    }
  }

  return(articles)
}

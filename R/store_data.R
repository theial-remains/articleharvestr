#' Clean Published Dates in Dataframe or List
#'
#' Detects and standardizes the format of each date to "YYYY-MM-DD".
#'
#' @param data A dataframe or a list with at least a 'published_date' field.
#' @return The same structure with 'published_date' cleaned.
#' @import lubridate
#' @export
sd_clean_date <- function(data) {
  # handle lists and data frames
  is_df <- is.data.frame(data)
  is_list <- is.list(data) && !is_df

  if (!(is_df || is_list)) {
    stop("Input must be a dataframe or a named list with 'published_date'.")
  }

  if (!"published_date" %in% names(data)) {
    stop("Input must contain 'published_date'.")
  }

  # make sure date column is character
  data$published_date <- as.character(data$published_date)

  clean_dates <- sapply(data$published_date, function(date_string) {
    if (is.na(date_string) || date_string == "") return(NA)

    if (grepl("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", date_string)) {
      return(as.character(as.Date(ymd_hms(date_string))))
    }

    # remove common timezone suffixes
    clean_string <- gsub("( EST| EDT| PDT| CDT| PST| CST| UTC| GMT| Z)$", "", date_string)

    possible_formats <- guess_formats(clean_string, orders = c(
      "ymd", "mdy", "dmy",
      "Ymd HMS", "mdy HMS", "dmy HMS",
      "Y-m-d H:M:S", "b d Y", "b d, Y H:M p", "b d, Y",
      "b d, Y H:M p", "b d, Y H:M", "b d, Y H:M:S p"
    ))

    if (length(possible_formats) == 0) return(clean_string)

    parsed <- tryCatch({
      as.Date(parse_date_time2(clean_string, orders = possible_formats[1]))
    }, error = function(e) NA)

    return(as.character(parsed))
  })

  data$published_date <- as.character(clean_dates)

  # for data frames: keep the same columns
  if (is_df) {
    return(data)
  }

  # for lists: return updated list
  return(data)
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
sd_clean_author <- function(dataframe,
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
sd_store_articles <- function(article_data, news_site, overwrite = FALSE) {
  if (!"url" %in% names(article_data) || !"published_date" %in% names(article_data)) {
    stop("article_data must contain at least 'url' and 'published_date'")
  }

  article_data$published_date <- as.character(article_data$published_date)

  # if full text is present, store both index and monthly jsons
  if ("text" %in% names(article_data) && any(!is.na(article_data$text) & article_data$text != "")) {
    text_data <- article_data[!is.na(article_data$text) & article_data$text != "", ]

    monthly_pipeline <- compose_storage(
      store_ensure_folders,
      store_index_json,
      store_monthly_json,
      store_base()
    )
    monthly_pipeline(text_data, news_site, overwrite)
  } else {
    # otherwise only store metadata in index.json
    index_pipeline <- compose_storage(
      store_ensure_folders,
      store_index_json,
      store_base()
    )
    index_pipeline(article_data, news_site, overwrite)
  }

  message("Storage complete for news site: ", news_site)
  invisible(NULL)
}

#' Pull articles from JSON files or index metadata
#'
#' Automatically selects data source (index or monthly JSONs) based on desired filters.
#'
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param news_site News site name (e.g., "huffpost")
#' @param scraped If TRUE/FALSE, filter by presence/absence of article text
#' @param sentiment If TRUE/FALSE, filter by presence/absence of sentiment data
#' @param url If TRUE, returns only url and published_date columns (ignores other filters)
#' @return A tibble of filtered article data
#' @export
sd_pull_articles <- function(start_date,
                             end_date,
                             news_site,
                             scraped = NULL,
                             sentiment = NULL,
                             url = FALSE) {
  folder <- file.path("inst/extdata/article_data", news_site)

  # force logic if sentiment is set
  if (!is.null(sentiment)) scraped <- TRUE
  if (url) scraped <- NULL; sentiment <- NULL

  # helper to load json files idk why I did this
  safe_read <- function(path) {
    if (file.exists(path)) {
      jsonlite::read_json(path, simplifyVector = TRUE)
    } else {
      tibble::tibble()
    }
  }

  index_path <- file.path(folder, "index.json")
  index_data <- safe_read(index_path)
  index_data$source <- "index"

  # load monthly jsons
  all_months <- list.files(folder, pattern = "\\d{4}-\\d{2}\\.json$", full.names = TRUE)
  monthly_data <- purrr::map_dfr(all_months, ~ {
    df <- safe_read(.x)
    if (nrow(df) > 0) df$source <- "monthly"
    df
  })

  # filter by date range
  all_data <- dplyr::bind_rows(index_data, monthly_data)
  all_data$published_date <- as.Date(all_data$published_date)

  all_data <- dplyr::filter(all_data,
    published_date >= as.Date(start_date) &
    published_date <= as.Date(end_date)
  )

  if (nrow(all_data) == 0) {
    stop("No articles found in the given date range.")
  }

  # url only mode logic
  # skip filters, return url and published_date
  if (url) {
    message("Returning URLs only from index JSON.")
    result <- dplyr::distinct(all_data, url, published_date)
    message(nrow(result), " article URLs found.")
    return(result)
  }

  # more logic
  if (!is.null(scraped)) {
    if (scraped) {
      all_data <- dplyr::filter(all_data, !is.na(text) & text != "")
    } else {
      all_data <- dplyr::filter(all_data, is.na(text) | text == "")
    }

    if (nrow(all_data) == 0) {
      stop("No articles match the 'scraped = ", scraped, "' condition in this date range.")
    }
  }

  if (!is.null(sentiment)) {
    if (sentiment) {
      all_data <- dplyr::filter(all_data, !is.na(sentiment_val))
    } else {
      all_data <- dplyr::filter(all_data, is.na(sentiment_val))
    }

    if (nrow(all_data) == 0) {
      stop("No articles match the 'sentiment = ", sentiment, "' condition in this date range.")
    }
  }

  # reporting messages n stuff
  # TODO add a verbose param
  total <- nrow(all_data)
  num_scraped <- sum(!is.na(all_data$text) & all_data$text != "")
  num_sentimented <- sum(!is.na(all_data$sentiment_val))
  num_url_only <- total - num_scraped

  message("Returned ", total, " articles:")
  message("Returning", num_url_only, " URL-only (not scraped)")
  message("Returning", num_scraped, " scraped with text")
  message("Returning", num_sentimented, " with sentiment data")

  return(all_data)
}

#' Sample random URLs per day/month/year from index.json
#'
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param news_site Name of the news site (e.g., "huffpost")
#' @param number Number of URLs to sample per time period
#' @param period Time period to group by: "day", "month", or "year"
#' @return A tibble with randomly sampled URLs and published_date
#' @export
sd_sample_urls <- function(start_date,
                           end_date,
                           news_site,
                           number,
                           period = c("day", "month", "year")) {
  period <- match.arg(period)
  folder <- file.path("inst/extdata/article_data", news_site)
  index_path <- file.path(folder, "index.json")

  if (!file.exists(index_path)) stop("index.json does not exist for site: ", news_site)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    stop("Start and end dates must be valid 'YYYY-MM-DD' format.")
  }

  # idiot proof start and end date depending on period
  if (period == "month") {
    start_date <- as.Date(format(start_date, "%Y-%m-01"))
    end_date <- lubridate::ceiling_date(as.Date(end_date), unit = "month") - 1
  } else if (period == "year") {
    start_date <- as.Date(format(start_date, "%Y-01-01"))
    end_date <- lubridate::ceiling_date(as.Date(end_date), unit = "year") - 1
  }

  # validate number
  if (!is.numeric(number) || number <= 0 || number != as.integer(number)) {
    stop("`number` must be a positive integer.")
  }

  # load index
  index <- jsonlite::read_json(index_path, simplifyVector = TRUE)

  # filter by date
  index <- dplyr::filter(index,
                         as.Date(published_date) >= start_date,
                         as.Date(published_date) <= end_date)

  if (nrow(index) == 0) {
    stop("No URLs found in the specified date range (", start_date, " to ", end_date, ").")
  }

  # group by chosen time period
  index$group <- dplyr::case_when(
    period == "day" ~ format(as.Date(index$published_date), "%Y-%m-%d"),
    period == "month" ~ format(as.Date(index$published_date), "%Y-%m"),
    period == "year" ~ format(as.Date(index$published_date), "%Y")
  )

  # check if each group has enough data
  group_counts <- index %>%
    dplyr::group_by(group) %>%
    dplyr::tally()

  insufficient <- group_counts %>% dplyr::filter(n < number)

  if (nrow(insufficient) > 0) {
    stop("Not enough articles to sample ", number, " in the following ", period, "(s): ",
         paste(insufficient$group, collapse = ", "))
  }

  # sample from each group
  sampled <- index %>%
    dplyr::group_by(group) %>%
    dplyr::slice_sample(n = number) %>%
    dplyr::ungroup() %>%
    dplyr::select(url, published_date)

  message("Sampled ", nrow(sampled), " article URLs (", number, " per ", period, ").")
  return(sampled)
}

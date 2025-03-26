#' Extract Links from a Sitemap (Supports XML and HTML)
#'
#' Fetches a sitemap, detects XML or HTML format, and extracts links.
#'
#' @param sitemap_url The URL of the sitemap.
#' @return A list containing extracted URLs and a string indicating format type ("html" or "xml").
#' @import xml2
#' @import rvest
#' @export
gu_extract_sitemap_links <- function(sitemap_url) {
  content_html <- tryCatch(read_html(sitemap_url), error = function(e) NULL)

  if (!is.null(content_html)) {
    links <- content_html %>% html_nodes("a[href]") %>% html_attr("href")

    if (length(links) > 0) {
      return(list(links = unique(links), format = "html"))
    }
  }

  # fallback: try parsing as XML
  content_xml <- tryCatch(read_xml(sitemap_url), error = function(e) NULL)

  if (!is.null(content_xml)) {
    no_ns <- xml_ns_strip(content_xml)
    links <- xml_find_all(no_ns, ".//loc") %>% xml_text()

    if (length(links) > 0) {
      return(list(links = links, format = "xml"))
    }
  }

  message("No links found in sitemap: ", sitemap_url)
  return(list(links = character(0), format = NA))
}

library(stringr)

#' Filter Links by Date
#'
#' Filters sitemap links based on the level and date range.
#'
#' @param links A character vector of sitemap links.
#' @param level The depth of the sitemap (1 = days, 2 = months, 3 = years).
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @return A character vector of filtered links.
#' @import stringr
#' @export
gu_filter_links_by_date <- function(links, level, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # regex patterns for filtering links directly
  date_pattern <- switch(as.character(level),
    "1" = "(/\\d{1,2}/?$|\\d{4}-\\d{2}-\\d{2})",  # level 1 (days)
    "2" = "(january|february|march|april|may|june|july|august|september|october|november|december|\\d{4}-\\d{2})",  # level 2 (months)
    "3" = "/(19[5-9][0-9]|20[0-9][0-9])/?$",  # level 3 (years)
    NULL
  )

  if (is.null(date_pattern)) {
    stop("Invalid level. Must be 1 (days), 2 (months), or 3 (years).")
  }

  matching_links <- links[str_detect(links, regex(date_pattern, ignore_case = TRUE))]
  parsed_dates <- NA

  # TODO: instead of this bullshit, have it:
  # level 1:
    #
  if (level == 1) {
    parsed_dates <- suppressWarnings(as.Date(str_extract(matching_links, "\\d{4}-\\d{2}-\\d{2}"), format = "%Y-%m-%d"))
  } else if (level == 2) {
    extracted_months <- str_extract(matching_links, "\\d{4}-\\d{2}")
    parsed_dates <- suppressWarnings(as.Date(paste0(extracted_months, "-01"), format = "%Y-%m-%d"))
  } else if (level == 3) {
    extracted_years <- str_extract(matching_links, "\\d{4}")
    parsed_dates <- suppressWarnings(as.Date(paste0(extracted_years, "-01-01"), format = "%Y-%m-%d"))
  }

  filtered_links <- matching_links[!is.na(parsed_dates) & parsed_dates >= start_date & parsed_dates <= end_date]

  filtered_links <- ifelse(str_starts(filtered_links, "https?://"),
                           filtered_links,
                           paste0("https:", filtered_links))
  return(filtered_links)
}

#' Recursively Fetch Article URLs from Sitemap
#'
#' Fetches article URLs from a sitemap, filtering at each level and processing all valid sitemaps.
#'
#' @param sitemap_url The URL of the sitemap.
#' @param levels The number of levels in the sitemap (1 = days, 2 = months, 3 = years).
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @param verbose Logical; if TRUE, prints execution time (default: TRUE).
#' @return A tibble with columns: url, published_date
#' @import stringr
#' @import tictoc
#' @importFrom tibble tibble
#' @export
gu_fetch_sitemap_articles <- function(sitemap_url, levels, start_date, end_date, verbose = TRUE) {
  if (verbose) tic()

  message("Fetching sitemap: ", sitemap_url, " (Levels: ", levels, ")")

  extracted_data <- gu_extract_sitemap_links(sitemap_url)
  all_links <- extracted_data$links

  if (length(all_links) == 0) {
    message("No links found in sitemap: ", sitemap_url)
    if (verbose) toc()
    return(tibble::tibble(url = character(0), published_date = as.Date(character(0))))
  }

  filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)

  if (length(filtered_links) == 0) {
    message("No links remain after date filtering at level ", levels)
    if (verbose) toc()
    return(tibble::tibble(url = character(0), published_date = as.Date(character(0))))
  }

  results <- list()

  if (levels == 1) {
    message("Extracting article links from final sitemap level...")

    for (link in filtered_links) {
      extracted_data <- gu_extract_sitemap_links(link)
      nested_links <- extracted_data$links

      # get published_date from the day level sitemap url
      date_guess <- str_extract(link, "\\d{4}-\\d{2}-\\d{2}")
      if (!is.na(date_guess)) {
        published_date <- as.Date(date_guess)
      } else {
        published_date <- NA
      }

      if (length(nested_links) > 0) {
        article_df <- tibble::tibble(
          url = nested_links,
          published_date = published_date
        )
        results[[length(results) + 1]] <- article_df
      }
    }

    final_result <- dplyr::bind_rows(results)
    message("Returning ", nrow(final_result), " articles from level 1.")

    if (verbose) {
      elapsed_time <- toc(quiet = TRUE)
      message(sprintf("Scraped in %.2f seconds.", elapsed_time$toc - elapsed_time$tic))
    }

    return(final_result)
  }

  for (link in filtered_links) {
    extracted_data <- gu_extract_sitemap_links(link)
    nested_links <- extracted_data$links

    if (length(nested_links) > 0) {
      filtered_nested_links <- gu_filter_links_by_date(nested_links, levels - 1, start_date, end_date)

      for (nested_link in filtered_nested_links) {
        nested_df <- gu_fetch_sitemap_articles(nested_link, levels - 1, start_date, end_date, verbose = FALSE)
        results[[length(results) + 1]] <- nested_df
      }
    }
  }

  final_result <- dplyr::bind_rows(results)

  if (verbose) {
    elapsed_time <- toc(quiet = TRUE)
    message(sprintf("Scraped %d articles in %.2f seconds.", nrow(final_result), elapsed_time$toc - elapsed_time$tic))
  }

  return(final_result)
}

#' Remove Duplicate URLs Based on Existing JSONs for a News Site
#'
#' Checks index.json or monthly JSONs depending on input data type, and removes
#' any URLs that already exist in stored files.
#'
#' @param url_data A tibble with at least 'url' and 'published_date'.
#' @param sitemap (Optional) The sitemap URL to help determine the news site.
#' @return A tibble of new (non-duplicate) URLs, or NULL if none remain.
#' @export
gu_remove_duplicates <- function(url_data, sitemap = NULL) {
  if (!all(c("url", "published_date") %in% names(url_data))) {
    stop("Input tibble must contain 'url' and 'published_date' columns.")
  }

  total_input <- nrow(url_data)
  if (total_input == 0) {
    message("No URLs provided.")
    return(NULL)
  }

  extract_news_site <- function(x) {
    domain <- sub("https?://(www\\.)?", "", x)
    domain <- sub("/.*", "", domain)
    domain <- sub("\\..*$", "", domain)
    tolower(domain)
  }

  news_site <- if (!is.null(sitemap)) {
    extract_news_site(sitemap)
  } else {
    extract_news_site(url_data$url[1])
  }

  dev_mode <- !nzchar(system.file(package = "articleharvestr"))
  base_path <- if (dev_mode) {
    file.path("inst/extdata/article_data", news_site)
  } else {
    file.path(system.file("extdata", "article_data", package = "articleharvestr"), news_site)
  }

  if (!dir.exists(base_path)) {
    message("No existing folder found for ", news_site, ". Returning all ", total_input, " URLs.")
    return(url_data)
  }

  has_text <- "text" %in% names(url_data)
  has_sentiment <- "sentiment_val" %in% names(url_data)

  # ----------- Case 1: Only url and published_date → check index.json ------------
  if (!has_text && !has_sentiment) {
    index_path <- file.path(base_path, "index.json")
    if (!file.exists(index_path)) {
      message("No index.json found for ", news_site, ". Returning all URLs.")
      return(url_data)
    }

    existing <- jsonlite::read_json(index_path, simplifyVector = TRUE)
    if (!"url" %in% names(existing)) {
      stop("index.json does not contain 'url' column.")
    }

    deduped <- dplyr::anti_join(url_data, dplyr::select(existing, url), by = "url")
  }

  # ----------- Case 2: Input has text (scraped) → check monthly JSONs ------------
  if (has_text && !has_sentiment) {
    url_data <- dplyr::mutate(url_data, month = format(as.Date(published_date), "%Y-%m"))
    months <- unique(url_data$month)

    scraped_urls <- character(0)

    for (m in months) {
      json_path <- file.path(base_path, paste0(m, ".json"))
      if (file.exists(json_path)) {
        articles <- tryCatch(
          jsonlite::read_json(json_path, simplifyVector = TRUE),
          error = function(e) NULL
        )
        if (!is.null(articles) && "url" %in% names(articles) && "text" %in% names(articles)) {
          scraped_urls <- c(scraped_urls, articles$url[!is.na(articles$text) & articles$text != ""])
        }
      }
    }

    deduped <- dplyr::anti_join(url_data, tibble::tibble(url = scraped_urls), by = "url")
  }

  # ----------- Case 3: Input has sentiment → remove if already sentimented -------
  if (has_sentiment) {
    url_data <- dplyr::mutate(url_data, month = format(as.Date(published_date), "%Y-%m"))
    months <- unique(url_data$month)

    sentimented_urls <- character(0)

    for (m in months) {
      json_path <- file.path(base_path, paste0(m, ".json"))
      if (file.exists(json_path)) {
        articles <- tryCatch(
          jsonlite::read_json(json_path, simplifyVector = TRUE),
          error = function(e) NULL
        )
        if (!is.null(articles) && "url" %in% names(articles) && "sentiment_val" %in% names(articles)) {
          sentimented_urls <- c(sentimented_urls, articles$url[!is.na(articles$sentiment_val)])
        }
      }
    }

    deduped <- dplyr::anti_join(url_data, tibble::tibble(url = sentimented_urls), by = "url")
  }

  removed <- nrow(url_data) - nrow(deduped)
  message("Removed ", removed, " duplicates. ", nrow(deduped), " new articles remain.")

  if (nrow(deduped) == 0) {
    message("No new articles to process.")
    return(NULL)
  }

  return(dplyr::select(deduped, url, published_date))
}
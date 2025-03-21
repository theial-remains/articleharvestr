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
#' @return A tibble with a single column: url
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
    return(tibble::tibble(url = character(0)))
  }

  filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)

  if (length(filtered_links) == 0) {
    message("No links remain after date filtering at level ", levels)
    if (verbose) toc()
    return(tibble::tibble(url = character(0)))
  }

  final_links <- character(0)

  if (levels == 1) {
    message("Extracting article links from final sitemap level...")

    for (link in filtered_links) {
      extracted_data <- gu_extract_sitemap_links(link)
      nested_links <- extracted_data$links
      final_links <- c(final_links, nested_links)
    }

    message("Returning ", length(final_links), " articles from level 1.")
  } else {
    for (link in filtered_links) {
      extracted_data <- gu_extract_sitemap_links(link)
      nested_links <- extracted_data$links

      if (length(nested_links) > 0) {
        filtered_nested_links <- gu_filter_links_by_date(nested_links, levels - 1, start_date, end_date)

        for (nested_link in filtered_nested_links) {
          new_links <- gu_fetch_sitemap_articles(nested_link, levels - 1, start_date, end_date, verbose = FALSE)
          final_links <- c(final_links, new_links$url)
        }
      }
    }
  }

  if (verbose) toc()

  return(tibble::tibble(url = final_links))
}

#' Remove Duplicate URLs Based on Existing CSV for a News Site
#'
#' Loads the site-specific CSV and removes any URLs from the new tibble that already exist in the CSV.
#'
#' @param url_data A tibble with at least a 'url' column (from gu_fetch_sitemap_articles()).
#' @param sitemap (Optional) The sitemap URL to help determine the news site.
#' @return A tibble of new (non-duplicate) URLs, or NULL if none remain.
#' @importFrom readr read_csv
#' @importFrom dplyr anti_join select
#' @export
gu_remove_duplicates <- function(url_data, sitemap = NULL) {
  if (!"url" %in% names(url_data)) {
    stop("Input must be a tibble with a 'url' column.")
  }

  total_input <- nrow(url_data)

  if (total_input == 0) {
    message("No URLs provided.")
    return(NULL)
  }

  # helper: extract domain from url or sitemap
  extract_news_site <- function(x) {
    domain <- sub("https?://(www\\.)?", "", x)
    domain <- sub("/.*", "", domain)
    domain <- sub("\\..*$", "", domain)
    return(tolower(domain))
  }

  # get news site
  news_site <- if (!is.null(sitemap)) {
    extract_news_site(sitemap)
  } else {
    extract_news_site(url_data$url[1])
  }

  # load csv
  dev_mode <- !nzchar(system.file(package = "articleharvestr"))
  folder_path <- if (dev_mode) "inst/extdata/article_data/" else system.file("extdata", "article_data", package = "articleharvestr")
  csv_path <- file.path(folder_path, paste0(news_site, ".csv"))

  if (!file.exists(csv_path)) {
    message("No existing CSV for ", news_site, ". Returning all ", total_input, " URLs.")
    return(url_data)
  }

  existing_data <- readr::read_csv(csv_path, show_col_types = FALSE)

  if (!"url" %in% names(existing_data)) {
    stop("CSV file for ", news_site, " must contain a 'url' column.")
  }

  # rm duplicates
  filtered <- dplyr::anti_join(url_data, dplyr::select(existing_data, url), by = "url")
  remaining <- nrow(filtered)
  removed <- total_input - remaining

  message("Removed ", removed, " duplicates. ", remaining, " new articles remain.")

  if (remaining == 0) {
    message("No new articles to process.")
    return(NULL)
  }

  return(filtered)
}

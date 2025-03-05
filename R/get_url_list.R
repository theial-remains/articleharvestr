#' Extract Links from a Sitemap (Supports XML and HTML)
#'
#' Fetches a sitemap, detects XML or HTML format, and extracts links.
#'
#' @param sitemap_url The URL of the sitemap.
#' @return A list containing extracted URLs and a string indicating format type ("xml" or "html").
#' @import httr
#' @import xml2
#' @import rvest
#' @export
gu_extract_sitemap_links <- function(sitemap_url) {
  response <- tryCatch(GET(sitemap_url), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  if (is.null(response) || http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content: ", sitemap_url)
    return(list(links = character(0), format = NA))
  }

  raw_content <- content(response, as = "text", encoding = "UTF-8")
  content_type <- headers(response)$`content-type`

  content_xml <- tryCatch(read_xml(raw_content), error = function(e) NULL)

  if (!is.null(content_xml)) {
    no_ns <- xml_ns_strip(content_xml)
    links <- xml_find_all(no_ns, ".//loc") %>% xml_text()

    if (length(links) > 0) {
      return(list(links = links, format = "xml"))
    }
  }

  content_html <- tryCatch(read_html(raw_content), error = function(e) NULL)

  if (!is.null(content_html)) {
    links <- content_html %>% html_nodes("a[href]") %>% html_attr("href")

    if (length(links) > 0) {
      return(list(links = unique(links), format = "html"))
    }
  }

  message("No links found in sitemap: ", sitemap_url)
  return(list(links = character(0), format = NA))
}

#' Filter Links by Date
#'
#' Filters sitemap links based on the level and date range.
#'
#' @param links A character vector of sitemap links.
#' @param level The depth of the sitemap (0 = articles, 1 = days, 2 = months, 3 = years).
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @return A character vector of filtered links.
#' @import stringr
#' @export
gu_filter_links_by_date <- function(links, level, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  date_pattern <- switch(as.character(level),
                         "3" = "\\d{4}",              # yearly
                         "2" = "\\d{4}-\\d{2}",       # monthly
                         "1" = "\\d{4}-\\d{2}-\\d{2}",# daily
                         NULL)  # articles don't need filtering

  if (!is.null(date_pattern)) {
    extracted_dates <- str_extract(links, date_pattern)
    parsed_dates <- suppressWarnings(as.Date(extracted_dates,
                                             format = "%Y-%m-%d"))

    if (level == 2) {
      parsed_dates <- suppressWarnings(as.Date(paste0(extracted_dates, "-01"),
                                               format = "%Y-%m-%d"))
    } else if (level == 3) {
      parsed_dates <- suppressWarnings(as.Date(paste0(extracted_dates, "-01-01"),
                                               format = "%Y-%m-%d"))
    }

    links <- links[!is.na(parsed_dates) & parsed_dates >= start_date & parsed_dates <= end_date]
  }

  return(links)
}

#' Recursively Fetch Article URLs from Sitemap
#'
#' Fetches article URLs from a sitemap, filtering at each level and processing all valid sitemaps.
#'
#' @param sitemap_url The URL of the sitemap.
#' @param levels The number of levels in the sitemap (1 = days, 2 = months, 3 = years).
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @return A character vector of article URLs.
#' @import stringr
#' @export
gu_fetch_sitemap_articles <- function(sitemap_url, levels, start_date, end_date) {
  message("Fetching sitemap: ", sitemap_url, " (Levels: ", levels, ")")

  extracted_data <- gu_extract_sitemap_links(sitemap_url)
  all_links <- extracted_data$links

  if (length(all_links) == 0) {
    message("No links found in sitemap: ", sitemap_url)
    return(character(0))
  }

  filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)

  if (length(filtered_links) == 0) {
    message("No links remain after date filtering at level ", levels)
    return(character(0))
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
    return(final_links)
  }

  for (link in filtered_links) {
    extracted_data <- gu_extract_sitemap_links(link)
    nested_links <- extracted_data$links

    if (levels > 1 && length(nested_links) > 0) {
      filtered_nested_links <- gu_filter_links_by_date(nested_links, levels - 1, start_date, end_date)

      for (nested_link in filtered_nested_links) {
        new_links <- gu_fetch_sitemap_articles(nested_link, levels - 1, start_date, end_date)
        final_links <- c(final_links, new_links)
      }
    }
  }

  return(final_links)
}

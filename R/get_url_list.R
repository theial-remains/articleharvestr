# gets list of links from article sitemap.xml

#' Extract Links from a Sitemap (Supports XML and HTML)
#'
#' Fetches a sitemap, detects XML or HTML format, and extracts links accordingly.
#'
#' @param sitemap_url The URL of the sitemap.
#' @return A list containing extracted URLs and a string indicating format type ("xml" or "html").
#' @import httr
#' @import xml2
#' @import rvest
#' @export
extract_sitemap_links <- function(sitemap_url) {
  message("Fetching sitemap: ", sitemap_url)

  # Fetch the sitemap content
  response <- tryCatch(GET(sitemap_url), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  if (is.null(response) || http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content: ", sitemap_url)
    return(list(links = character(0), format = NA))
  }

  # Get content as text
  raw_content <- content(response, as = "text")
  content_type <- headers(response)$`content-type`

  # Try parsing as XML first
  content_xml <- tryCatch(read_xml(raw_content), error = function(e) NULL)

  if (!is.null(content_xml)) {
    # Handle namespaces in XML and extract <loc> elements
    no_ns <- xml_ns_strip(content_xml)
    links <- xml_find_all(no_ns, ".//loc") %>% xml_text()

    if (length(links) > 0) {
      return(list(links = links, format = "xml"))
    }
  }

  # If XML fails, try parsing as HTML
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
filter_links_by_date <- function(links, level, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Determine the appropriate date pattern for filtering
  date_pattern <- switch(level,
                         "3" = "\\d{4}",              # Yearly
                         "2" = "\\d{4}-\\d{2}",       # Monthly
                         "1" = "\\d{4}-\\d{2}-\\d{2}",# Daily
                         NULL)  # Articles don't need filtering

  if (!is.null(date_pattern)) {
    # Extract dates from links
    link_dates <- as.Date(str_extract(links, date_pattern), format = "%Y-%m-%d")

    # Filter links by date range
    links <- links[!is.na(link_dates) & link_dates >= start_date & link_dates <= end_date]
  }

  return(links)
}

#' Fetch Article URLs from Sitemap
#'
#' Recursively extracts article URLs from a sitemap, filtering results at each level.
#'
#' @param sitemap_url The URL of the sitemap.
#' @param levels The number of levels in the sitemap (0 = articles, 1 = days, 2 = months, 3 = years).
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @return A character vector of article URLs.
#' @import stringr
#' @export
fetch_sitemap_articles <- function(sitemap_url, levels, start_date, end_date) {
  message("Fetching sitemap: ", sitemap_url, " (Levels: ", levels, ")")

  # Extract links
  extracted_data <- extract_sitemap_links(sitemap_url)
  all_links <- extracted_data$links

  if (length(all_links) == 0) {
    message("No links found in sitemap: ", sitemap_url)
    return(character(0))
  }

  # Filter links by the appropriate date format
  filtered_links <- filter_links_by_date(all_links, levels, start_date, end_date)

  if (length(filtered_links) == 0) {
    message("No links remain after date filtering at level ", levels)
    return(character(0))
  }

  # If we're at the article level, return links
  if (levels == 0) {
    message("Reached article level. Returning ", length(filtered_links), " articles.")
    return(filtered_links)
  }

  # Recursively process the next level
  final_links <- character(0)
  for (link in filtered_links) {
    new_links <- fetch_sitemap_articles(link, levels - 1, start_date, end_date)
    final_links <- c(final_links, new_links)
  }

  return(final_links)
}
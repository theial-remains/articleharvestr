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
gu_extract_sitemap_links <- function(sitemap_url) {
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
gu_filter_links_by_date <- function(links, level, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Determine the appropriate regex pattern for extracting the date
  date_pattern <- switch(as.character(level),
                         "3" = "\\d{4}",              # Yearly
                         "2" = "\\d{4}-\\d{2}",       # Monthly
                         "1" = "\\d{4}-\\d{2}-\\d{2}",# Daily
                         NULL)  # Articles don't need filtering

  if (!is.null(date_pattern)) {
    # Extract the first date that matches the pattern from each link
    extracted_dates <- str_extract(links, date_pattern)

    # Convert extracted dates to Date format
    parsed_dates <- suppressWarnings(as.Date(extracted_dates, format = "%Y-%m-%d"))

    # Handle cases where monthly or yearly links are extracted
    if (level == 2) {
      parsed_dates <- suppressWarnings(as.Date(paste0(extracted_dates, "-01"), format = "%Y-%m-%d"))
    } else if (level == 3) {
      parsed_dates <- suppressWarnings(as.Date(paste0(extracted_dates, "-01-01"), format = "%Y-%m-%d"))
    }

    # Filter links by date range
    links <- links[!is.na(parsed_dates) & parsed_dates >= start_date & parsed_dates <= end_date]
  }

  return(links)
}

#' Recursively Fetch Article URLs from Sitemap with Date Filtering
#'
#' Extracts article URLs from a sitemap, filtering at each level and processing all valid sitemaps.
#'
#' @param sitemap_url The URL of the sitemap.
#' @param levels The number of levels in the sitemap (0 = articles, 1 = days, 2 = months, 3 = years).
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @return A character vector of article URLs.
#' @import stringr
#' @export
gu_fetch_sitemap_articles <- function(sitemap_url, levels, start_date, end_date) {
  # Extract links from sitemap
  extracted_data <- gu_extract_sitemap_links(sitemap_url)
  all_links <- extracted_data$links

  if (length(all_links) == 0) {
    message("No links found in sitemap: ", sitemap_url)
    return(character(0))
  }

  # **Filter links by date at the current level**
  filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)

  if (length(filtered_links) == 0) {
    message("No links remain after date filtering at level ", levels)
    return(character(0))
  }

  # **Separate sitemap links (XML) from article links (non-XML)**
  sitemap_links <- filtered_links[str_detect(filtered_links, "\\.xml$")]
  article_links <- setdiff(filtered_links, sitemap_links)

  # **Base Case: If levels == 0 or no sitemap links remain, return articles**
  if (levels == 0 || length(sitemap_links) == 0) {
    message("Reached article level. Returning ", length(article_links), " articles.")
    return(article_links)
  }

  # **Recursive Case: Extract and process all valid sitemap links**
  final_links <- character(0)
  for (link in sitemap_links) {
    extracted_data <- gu_extract_sitemap_links(link)  # Extract nested links
    nested_links <- extracted_data$links

    if (length(nested_links) > 0) {
      filtered_nested_links <- gu_filter_links_by_date(nested_links, levels - 1, start_date, end_date)

      for (nested_link in filtered_nested_links) {
        new_links <- gu_fetch_sitemap_articles(nested_link, levels - 1, start_date, end_date)
        final_links <- c(final_links, new_links)
      }
    }
  }

  return(final_links)
}

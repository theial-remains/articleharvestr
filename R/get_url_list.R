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

#' Fetch Article URLs by Sitemap Levels
#'
#' Processes sitemaps based on known levels (0 = direct to articles, 1 = daily, 2 = monthly, 3 = yearly).
#'
#' @param sitemap_url The URL of the sitemap.
#' @param levels The number of levels in the sitemap structure.
#' @param start_date The start date for filtering (YYYY-MM-DD).
#' @param end_date The end date for filtering (YYYY-MM-DD).
#' @return A character vector of article URLs.
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

  # If the sitemap is already at the article level, return the links
  if (levels == 0) {
    message("Article-level sitemap detected. Returning links.")
    return(all_links)
  }

  # Recursive processing based on sitemap depth
  filtered_links <- character(0)

  for (link in all_links) {
    new_links <- fetch_sitemap_articles(link, levels - 1, start_date, end_date)
    filtered_links <- c(filtered_links, new_links)
  }

  return(filtered_links)
}

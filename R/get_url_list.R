# gets list of links from article sitemap.xml
# Helper Functions ----------------------------------------------------------


#' Append Sitemap URL to Year Links
#'
#' This function takes the base sitemap URL and appends it to each year link to create full URLs.
#' @param url_prefix A character string representing the base URL of the sitemap (e.g., "https://www.example.com/sitemap/").
#' @param link_list A character vector containing relative year links (e.g., "1990/", "1991/").
#' @return A character vector of full URLs.
#' @export
gu_append_links <- function(url_prefix, link_list) {
  # Ensure the sitemap URL ends with a forward slash to avoid incorrect concatenation
  if (!grepl("/$", url_prefix)) {
    url_prefix <- paste0(url_prefix, "/")
  }

  # Append each year link to the sitemap URL
  full_urls <- paste0(url_prefix, link_list)

  return(full_urls)
}


#' Parse Sitemap Content
#'
#' This function parses the sitemap content based on its content type (XML or HTML).
#' It extracts URLs from XML <loc> elements or HTML <a> tags within a specified parent class.
#' @param content_text A character string representing the sitemap content.
#' @param content_type The content type (e.g., "application/xml" or "text/html").
#' @param ol_class An optional character string specifying the class of the <ol> tag containing links.
#' @return A list of URLs extracted from the sitemap, or NULL if an error occurs.
#' @export
gu_parse_sitemap <- function(content_text, content_type, ol_class = NULL) {
  if (grepl("xml", content_type, ignore.case = TRUE)) {
    # XML Parsing
    sitemap_xml <- tryCatch(read_xml(content_text), error = function(e) {
      message("Error reading XML: ", e)
      return(NULL)
    })

    if (is.null(sitemap_xml)) return(NULL)

    # Extract all <loc> elements (URL links) from the XML
    all_links <- sitemap_xml %>%
      xml_find_all(".//loc") %>%
      xml_text()

  } else if (grepl("html", content_type, ignore.case = TRUE)) {
    # HTML Parsing
    sitemap_html <- tryCatch(read_html(content_text), error = function(e) {
      message("Error reading HTML: ", e)
      return(NULL)
    })

    if (is.null(sitemap_html)) return(NULL)

    # Extract only <a> elements within a specific <ol> class, if provided
    if (!is.null(ol_class)) {
      all_links <- sitemap_html %>%
        html_nodes(paste0("ol.", ol_class, " a")) %>%  # Find <a> tags within specified <ol> class
        html_attr("href")                              # Extract href attributes
    } else {
      all_links <- sitemap_html %>%
        html_nodes("a") %>%                            # Find all <a> tags
        html_attr("href")                              # Extract href attributes
    }

  } else {
    message("Unsupported content type: ", content_type)
    return(NULL)
  }

  return(all_links)
}


#' Get Year Links
#'
#' Extracts a list of year links within a specified range from an XML or HTML sitemap.
#' @param sitemap_url A character string representing the URL of a sitemap containing year links.
#' @param year_min Minimum year to include.
#' @param year_max Maximum year to include.
#' @return A list of URLs of the year links.
#' @export
gu_year_links <- function(sitemap_url, year_min, year_max) {
  # Fetch URL content
  response <- tryCatch(httr::GET(sitemap_url), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  # Check if the request was successful
  if (is.null(response) || httr::http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content.")
    return(NULL)
  }

  # Determine the content type and get the content text
  content_type <- httr::headers(response)$`content-type`
  content_text <- httr::content(response, as = "text")

  # Use the helper function to parse the sitemap content
  all_links <- gu_parse_sitemap(content_text, content_type)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  # Filter links to those that contain a year within the specified range
  year_links <- all_links %>%
    purrr::keep(~ {
      year <- str_extract(.x, "\\d{4}")
      !is.na(year) && as.numeric(year) >= year_min && as.numeric(year) <= year_max
    })

  return(year_links)
}


#' Get Month Links
#'
#' Extracts a list of month links within a specified range from an XML or HTML sitemap.
#' @param month_link A character string representing the URL of a link containing month links.
#' @param month_min Minimum month to include (numeric, 1-12).
#' @param month_max Maximum month to include (numeric, 1-12).
#' @param ol_class An optional character string specifying the class of the <ol> tag containing links.
#' @return A list of URLs of the month links.
#' @export
gu_month_links <- function(month_link, month_min, month_max, ol_class = NULL) {
  # Fetch the content of the URL
  response <- tryCatch(httr::GET(month_link), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  # Check if the request was successful
  if (is.null(response) || httr::http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content.")
    return(NULL)
  }

  # Determine the content type and get the content text
  content_type <- httr::headers(response)$`content-type`
  content_text <- httr::content(response, as = "text")

  # Use the helper function to parse the sitemap content
  all_links <- gu_parse_sitemap(content_text = content_text,
                                content_type = content_type,
                                ol_class = ol_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  # Filter links to those that contain a month within the specified range
  month_links <- all_links %>%
    purrr::keep(~ {
      month <- str_extract(.x, "/\\d{4}/(0[1-9]|1[0-2])/")
      if (!is.na(month)) {
        month_num <- as.numeric(str_extract(month, "(0[1-9]|1[0-2])"))
        return(month_num >= month_min && month_num <= month_max)
      }
      return(FALSE)
    })

  return(all_links)
}


#' title
#'
#' description
#' @param day_link A character string representing the URL of a link contaning day links
#' @param day_min
#' @param day_max
#' @return A list of URLs of the day links
#' @export
gu_day_links <- function(day_link, day_min, day_max) {
  # code goes here
}


# Main Functions ----------------------------------------------------------


#' Scrape for links by date in a year-month-day-links website structure
#'
#' Use for websites with a year-month-day-links sitemap structure
#' @param sitemap_url A character string representing the sitemap URL of the website.
#' @param date_range
#' @return List of links for the specified date range
#' @export
gu_ymdl_str_links <- function(sitemap_url, date_range) {
  # xml > year links > month links > day links
}
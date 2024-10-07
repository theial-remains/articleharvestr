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
  # Ensure the URL ends with a forward slash to avoid incorrect concatenation
  if (!grepl("/$", url_prefix)) {
    url_prefix <- paste0(url_prefix, "/")
  }

  # Append each link to the URL
  full_urls <- paste0(url_prefix, link_list)

  return(full_urls)
}


#' Parse Sitemap Content
#'
#' This function parses the sitemap content based on its content type (XML or HTML).
#' It extracts URLs from XML <loc> elements or HTML <a> tags within a specified parent tag and class.
#' @param content_text A character string representing the sitemap content.
#' @param content_type The content type (e.g., "application/xml" or "text/html").
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs extracted from the sitemap, or NULL if an error occurs.
#' @export
gu_parse_sitemap <- function(content_text, content_type, tag_type, tag_class) {
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

    # Extract <a> elements within the specified tag and class, if provided
    if (!is.null(tag_class)) {
      # Use the tag_type and tag_class to form the CSS selector
      all_links <- sitemap_html %>%
        html_nodes(paste0(tag_type, ".", tag_class, " a")) %>%  # Find <a> tags within the specified tag and class
        html_attr("href")                                       # Extract href attributes
    } else {
      all_links <- sitemap_html %>%
        html_nodes("a") %>%                                     # Find all <a> tags
        html_attr("href")                                       # Extract href attributes
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
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the year links.
#' @export
gu_year_links <- function(sitemap_url, year_min, year_max, tag_type, tag_class) {
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
  all_links <- gu_parse_sitemap(content_text, content_type, tag_type = tag_type, tag_class = tag_class)

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
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the month links.
#' @export
gu_month_links <- function(month_link, tag_type, tag_class) {
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
  all_links <- gu_parse_sitemap(content_text = content_text, content_type = content_type, tag_type = tag_type, tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  return(all_links)
}


#' Get Day Links
#'
#' Extracts a list of day links from an XML or HTML sitemap.
#' @param day_link A character string representing the URL of a link containing day links.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the day links.
#' @export
gu_day_links <- function(day_link, tag_type, tag_class) {
  # Fetch the content of the URL
  response <- tryCatch(httr::GET(day_link), error = function(e) {
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
  all_links <- gu_parse_sitemap(content_text = content_text, content_type = content_type, tag_type = tag_type, tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  return(all_links)
}


#' Get Article Links
#'
#' Extracts a list of article links from an XML or HTML sitemap of a day.
#' @param day_link A character string representing the URL of a link containing article links.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the article links.
#' @export
gu_article_links <- function(day_link, tag_type, tag_class) {
  # Fetch the content of the URL
  response <- tryCatch(httr::GET(day_link), error = function(e) {
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
                                tag_type = tag_type,
                                tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  return(all_links)
}


#' Get All Month Links from a List of Year Links
#'
#' This function calls `gu_month_links` for each link in a list of year URLs and returns all month links.
#' @param year_links A character vector containing the list of year URLs.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of all the month links.
#' @export
gu_apply_month_links <- function(year_links, tag_type, tag_class) {
  # Use purrr::map to call gu_month_links for each year link
  all_month_links <- map(year_links, ~ {
    month_links <- gu_month_links(.x, tag_type = tag_type, tag_class = tag_class)

    # Check if the first month link contains the year link text
    if (length(month_links) > 0 && !grepl(.x, month_links[1])) {
      # If not, append the year_link text to the beginning of all month links
      month_links <- gu_append_links(.x, month_links)
    }

    return(month_links)
  })

  # Flatten the list of lists into a single vector of month links
  all_month_links <- flatten_chr(all_month_links)

  return(all_month_links)
}


#' Get All Day Links from a List of Month Links
#'
#' This function calls `gu_day_links` for each link in a list of month URLs and returns all day links.
#' @param month_links A character vector containing the list of month URLs.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of all the day links.
#' @export
gu_apply_day_links <- function(month_links, tag_type, tag_class) {
  # Use purrr::map to call gu_day_links for each month link
  all_day_links <- map(month_links, ~ {
    day_links <- gu_day_links(.x, tag_type = tag_type, tag_class = tag_class)

    # Check if the first day link contains the month link text
    if (length(day_links) > 0 && !grepl(.x, day_links[1])) {
      # If not, append the month_link text to the beginning of all day links
      day_links <- gu_append_links(.x, day_links)
    }

    return(day_links)
  })

  # Flatten the list of lists into a single vector of day links
  all_day_links <- flatten_chr(all_day_links)

  return(all_day_links)
}


#' Get All Article Links from a List of Day Links
#'
#' This function calls `gu_article_links` for each link in a list of day URLs and returns all article links.
#' @param day_links A character vector containing the list of day URLs.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of all the article links.
#' @export
gu_apply_article_links <- function(day_links, tag_type = "ol", tag_class = NULL) {
  # Use purrr::map to call gu_article_links for each day link
  all_article_links <- map(day_links, ~ {
    gu_article_links(.x, tag_type = tag_type, tag_class = tag_class)
  })

  # Flatten the list of lists into a single vector of article links
  all_article_links <- flatten_chr(all_article_links)

  return(all_article_links)
}


# Main Functions ----------------------------------------------------------


#' Scrape for links by date in a year-month-day-links website structure
#'
#' Use for websites with a year-month-day-links sitemap structure
#' @param sitemap_url A character string representing the sitemap URL of the website.
#' @param date_range Range of dates to return urls for
#' @return List of links for the specified date range
#' @export
gu_ymdl_str_links <- function(sitemap_url, date_range) {
  # xml > year links > month links > day links
}
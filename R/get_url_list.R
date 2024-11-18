# gets list of links from article sitemap.xml
# TODO make main function work for xmls that contain multiple years, multiple months

# XML Functions ----------------------------------------------------------
# Helper Functions ----------------------------------------------------------


#' Scrape Sitemap with Date in URL
#'
#' Parses an XML sitemap where the date information is part of the URL and filters links by a date range.
#'
#' @param sitemap_url The URL of the XML sitemap to parse.
#' @param start_date The start date for filtering (as "YYYY-MM-DD").
#' @param end_date The end date for filtering (as "YYYY-MM-DD").
#' @return A character vector of filtered links from the sitemap.
#' @import httr
#' @import xml2
#' @export
gu_parse_xml_sitemap_date_in_url <- function(sitemap_url, start_date, end_date) {
  # Fetch the sitemap content
  response <- tryCatch(GET(sitemap_url), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  if (is.null(response) || http_status(response)$category != "Success") {
    stop("Failed to retrieve the URL content: ", sitemap_url)
  }

  # Parse XML content
  raw_content <- content(response, as = "text")
  content_xml <- tryCatch(read_xml(raw_content), error = function(e) {
    message("Error parsing XML content: ", e)
    return(NULL)
  })

  if (is.null(content_xml)) return(character())

  # Handle namespaces in the XML, extract links
  no_ns <- xml_ns_strip(content_xml)
  links <- xml_find_all(no_ns, ".//loc") %>%
    xml_text()

  link_numbers <- seq_along(links)

  # Extract dates by removing all non-date characters
  link_dates <- sapply(links, function(link) {
    date_match <- regmatches(link, regexpr("\\d{4}-\\d{2}-\\d{2}", link))
    if (length(date_match) > 0) {
      return(as.Date(date_match))
    }
    return(NA)
  })

  # Filter by date range
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  valid_indices <- which(!is.na(link_dates) & link_dates >= start_date & link_dates <= end_date)

  # Return filtered links by their indices
  filtered_links <- links[valid_indices]

  return(filtered_links)
}


#' Scrape Sitemap with Dates in XML Tags
#'
#' Parses an XML sitemap where the date information is stored in an XML tag (e.g., `<lastmod>`) and filters links by a date range.
#'
#' @param sitemap_url The URL of the XML sitemap to parse.
#' @param start_date The start date for filtering (as "YYYY-MM-DD").
#' @param end_date The end date for filtering (as "YYYY-MM-DD").
#' @param tag The xml tag containing the date information for each link
#' @return A character vector of filtered links from the sitemap.
#' @import httr
#' @import xml2
#' @export
gu_parse_xml_sitemap_date_in_tag <- function(sitemap_url, start_date, end_date, tag) {
  # Fetch the sitemap content
  response <- tryCatch(GET(sitemap_url), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  if (is.null(response) || http_status(response)$category != "Success") {
    stop("Failed to retrieve the URL content: ", sitemap_url)
  }

  # Parse XML content
  raw_content <- content(response, as = "text")
  content_xml <- tryCatch(read_xml(raw_content), error = function(e) {
    message("Error parsing XML content: ", e)
    return(NULL)
  })

  if (is.null(content_xml)) return(character())

  # Handle namespaces in the XML and extract links and dates
  no_ns <- xml_ns_strip(content_xml)
  links <- xml_find_all(no_ns, ".//loc") %>% xml_text()
  dates <- xml_find_all(no_ns, tag) %>% xml_text()

  # Ensure links and dates align
  if (length(links) != length(dates)) {
    stop("Mismatch between number of links and dates.")
  }

  # Convert dates to Date objects
  link_dates <- sapply(dates, function(date) {
    tryCatch(as.Date(substr(date, 1, 10)), error = function(e) NA)
  })

  # Filter by date range
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  valid_indices <- which(!is.na(link_dates) & link_dates >= start_date & link_dates <= end_date)

  # Return filtered links by their indices
  filtered_links <- links[valid_indices]

  return(filtered_links)
}


#' Get Schema Information for a Website
#'
#' This helper function retrieves and processes schema information for a given website
#' to prepare for sitemap scraping.
#'
#' @param website_url A character string representing the URL of the website.
#' @return A data frame with columns "number", "type", and "class", where each row represents a layer.
#' @importFrom utils read.csv
#' @export
gu_get_schema_info <- function(website_url) {
  # Step 1: Retrieve the schema for the website
  schema <- gs_pull_schema(website_url)

  # Check if schema was found
  if (is.null(schema)) {
    stop("Schema not found for the given website.")
  }

  # Step 2: Create a data frame for layers
  schema_info <- data.frame(
    number = 1:4, # Representing layers 1–4
    type = c(schema$layer1_type, schema$layer2_type, schema$layer3_type, schema$layer4_type),
    class = c(schema$layer1_class, schema$layer2_class, schema$layer3_class, schema$layer4_class),
    stringsAsFactors = FALSE
  )

  # Step 3: Remove rows with only NA in "type" and "class"
  schema_info <- schema_info[!(is.na(schema_info$type) & is.na(schema_info$class)), ]

  # Step 4: Return the processed schema info
  return(schema_info)
}


#' Get Instructions for Each Layer of Sitemap Parsing
#'
#' This function determines the action and helper function for each layer
#' based on the schema info and whether the layer is the first or subsequent.
#'
#' @param layer_info A single-row data frame with columns "number", "type", and "class" for a layer.
#' @return A character string with instructions, e.g., "single gu_parse_xml_sitemap_date_in_url" or "map gu_parse_xml_sitemap_date_in_tag".
#' @export
gu_get_layer_instructions <- function(layer_info) {
  # Ensure the input is a single row
  if (nrow(layer_info) != 1) {
    stop("layer_info must be a single-row data frame.")
  }

  # Determine the helper function based on "type"
  helper_function <- switch(
    layer_info$type,
    url_date = "gu_parse_xml_sitemap_date_in_url",
    tag_date = "gu_parse_xml_sitemap_date_in_tag",
    stop(paste("Unsupported type:", layer_info$type))
  )

  return(paste(helper_function))
}


# Main Functions ----------------------------------------------------------


# TODO will not work with anything other than huffpost rn because I didnt add dynamic hanlding for the tag param
# FIXME error when sitemap urls do not have full ymd info. cannot be fucked to fix it rn
# easiest way to fix it would be to have the helper functions handle yyyy case ect.
# or make another helper function to do it for them and integrate that

#' Process Sitemap Layers
#'
#' This function processes up to four sitemap layers for a given website URL.
#' It dynamically retrieves schema information and applies helper functions
#' for processing links at each layer. The function supports error handling
#' and outputs processed links for each layer.
#'
#' @param website_url A string. The base URL of the website to process.
#' @param start_date A string. The start date for filtering sitemap links in "YYYY-MM-DD" format.
#' @param end_date A string. The end date for filtering sitemap links in "YYYY-MM-DD" format.
#'
#' @return A list where each element corresponds to the processed links at each layer.
#' @export
#'
#' @examples
#' # Example usage:
#' website_url <- "https://www.huffpost.com"
#' start_date <- "2015-01-01"
#' end_date <- "2015-01-04"
#' result_links <- gu_parse_xml(website_url, start_date, end_date)
#' print(result_links)
gu_parse_xml <- function(website_url, start_date, end_date) {
  # Fetch schema information and starting sitemap
  schema_info <- gu_get_schema_info(website_url)
  starting_sitemap <- gs_pull_schema(website_url)$starting_sitemap[1]

  # Initialize link lists
  link_lists <- list()

  # Layer 1
  link_lists[[1]] <- tryCatch({
    gu_parse_xml_sitemap_date_in_url(starting_sitemap, start_date, end_date)
  }, error = function(e) {
    message("Error in Layer 1:", e)
    return(character(0))
  })

  # Process subsequent layers dynamically
  for (layer in 2:min(4, nrow(schema_info))) {
    if (length(link_lists[[layer - 1]]) == 0) {
      stop(paste("No links to process for Layer", layer))
    }

    # Determine tag and helper function for the current layer
    tag <- paste0(".//", schema_info$class[layer])
    helper_function <- gu_parse_xml_sitemap_date_in_tag

    # Process links for the current layer
    link_lists[[layer]] <- purrr::map(link_lists[[layer - 1]], function(link) {
      tryCatch({
        helper_function(link, start_date, end_date, tag)
      }, error = function(e) {
        message("Error processing link (Layer", layer, "): ", link, " - ", e)
        return(character(0))
      })
    }) %>% unlist()
  }

  # Return the final set of links
  return(link_lists)
}


# HTML Functions ----------------------------------------------------------
# FIXME INCREDIBLY FUCKED UP html functions
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
#' @importFrom xml2 read_xml
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom stringr str_extract
#' @import tidyr
#' @export
gu_parse_sitemap <- function(content_text,
                             content_type,
                             tag_type = NULL,
                             tag_class = NULL) {
  if (grepl("xml", content_type, ignore.case = TRUE)) {
    # XML Parsing
    sitemap_xml <- tryCatch(read_xml(content_text), error = function(e) {
      message("Error reading XML: ", e)
      return(NULL)
    })

    # Extract all <loc> elements (URL links) from the XML
    ns <- xml_ns(sitemap_xml)
    ns_prefix <- names(ns)[1]  # This will get the prefix 'd1'

    loc_nodes <- xml_find_all(sitemap_xml, paste0(".//", ns_prefix, ":loc"), ns)
    all_links <- xml_text(loc_nodes)

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
        html_nodes(paste0(tag_type, ".", tag_class, " a")) %>%
        html_attr("href")
    } else {
      all_links <- sitemap_html %>%
        html_nodes("a") %>%
        html_attr("href")
    }

  } else {
    message("Unsupported content type: ", content_type)
    return(NULL)
  }

  return(all_links)
}


#' Filter Links by Date
#'
#' This helper function filters a vector of URLs based on the specified date level (e.g., "year", "month", "day").
#' It extracts dates from URLs in formats such as `YYYY`, `YYYY/MM`, or `YYYY/MM/DD`, allowing for
#' both forward slashes and hyphens. For day-level links, it further filters URLs by a specified date range.
#'
#' @param links A character vector of URLs to be filtered.
#' @param level A character string specifying the date level ("year", "month", or "day").
#' @param start_date The start date for filtering (as `Date`).
#' @param end_date The end date for filtering (as `Date`).
#' @return A character vector of URLs that match the specified date level and date range.
#' @importFrom stringr str_extract
#' @export
gu_filter_links_by_date <- function(links, level, start_date, end_date) {
  # Define pattern based on level to match year, month, or full date
  pattern <- switch(level,
                    "year" = "\\d{4}",                       # Matches 4-digit year
                    "month" = "\\d{4}[-/]\\d{2}",            # Matches year and month with / or -
                    "day" = "\\d{4}[-/]\\d{2}[-/]\\d{2}")    # Matches full date with / or -

  # Find all links that match the pattern
  links <- links[grepl(pattern, links)]

  # For full-date (day-level) links, apply date range filtering
  if (level == "day" || level == 1) {  # level==1 allows filtering if levels vector is just [1]
    # Extract date part (YYYY-MM-DD) and convert to Date format
    dates <- as.Date(str_extract(links, "\\d{4}[-/]\\d{2}[-/]\\d{2}"), format = "%Y-%m-%d")

    # Filter links by date range
    links <- links[!is.na(dates) & dates >= start_date & dates <= end_date]
  }

  return(links)
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
#' @import httr
#' @importFrom purrr keep
#' @importFrom stringr str_extract
#' @export
gu_year_links <- function(sitemap_url,
                          year_min,
                          year_max,
                          tag_type = NULL,
                          tag_class = NULL) {
  # Fetch URL content
  response <- tryCatch(GET(sitemap_url), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  # Check if the request was successful
  if (is.null(response) || http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content.")
    return(NULL)
  }

  # Determine the content type and get the content text
  content_type <- headers(response)$`content-type`
  content_text <- content(response, as = "text")

  # Use the helper function to parse the sitemap content
  all_links <- gu_parse_sitemap(content_text,
                                content_type,
                                tag_type = tag_type,
                                tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  # Filter links to those that contain a year within the specified range
  year_links <- all_links %>%
    keep(~ {
      year <- str_extract(.x, "\\d{4}")
      !is.na(year) && as.numeric(year) >= year_min && as.numeric(year) <= year_max
    })

  return(year_links)
}

#' Get Month Links
#'
#' Extracts a list of month links within a specified range from an XML or HTML sitemap.
#' @param year_link A character string representing the URL of a year link containing month links.
#' @param month_min The starting month of the range (e.g., 1 for January).
#' @param month_max The ending month of the range (e.g., 12 for December).
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the month links.
#' @import httr
#' @importFrom stringr str_extract
#' @export
gu_month_links <- function(year_link, month_min = 1, month_max = 12, tag_type = NULL, tag_class = NULL) {
  # Fetch the content of the URL
  response <- tryCatch(GET(year_link), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  # Check if the request was successful
  if (is.null(response) || http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content.")
    return(NULL)
  }

  # Determine the content type and get the content text
  content_type <- headers(response)$`content-type`
  content_text <- content(response, as = "text")

  # Use the helper function to parse the sitemap content
  all_links <- gu_parse_sitemap(content_text = content_text,
                                content_type = content_type,
                                tag_type = tag_type,
                                tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  # Filter the links based on the specified month range
  filtered_links <- keep(all_links, ~ {
    month <- as.numeric(str_extract(.x, "^\\d{2}(?=/)"))
    message("Checking month in link ", .x, ": extracted month = ", month)
    !is.na(month) && month >= month_min && month <= month_max
  })

  return(filtered_links)
}


#' Get Day Links
#'
#' Extracts a list of day links within a specified range from an XML or HTML sitemap.
#' @param month_link A character string representing the URL of a month link containing day links.
#' @param day_min The starting day of the range (e.g., 1 for the 1st).
#' @param day_max The ending day of the range (e.g., 31 for the 31st).
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the day links.
#' @import httr
#' @export
gu_day_links <- function(month_link, day_min = 1, day_max = 31, tag_type = NULL, tag_class = NULL) {
  # Fetch the content of the URL
  response <- tryCatch(GET(month_link), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  # Check if the request was successful
  if (is.null(response) || http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content.")
    return(NULL)
  }

  # Determine the content type and get the content text
  content_type <- headers(response)$`content-type`
  content_text <- content(response, as = "text")

  # Use the helper function to parse the sitemap content
  all_links <- gu_parse_sitemap(content_text = content_text,
                                content_type = content_type,
                                tag_type = tag_type,
                                tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  # Filter the links based on the specified day range
  filtered_links <- keep(all_links, ~ {
    day <- as.numeric(str_extract(.x, "^\\d{2}")) # Extract day of link
    !is.na(day) && day >= day_min && day <= day_max # Keep if day in range
  })

  return(filtered_links)
}


#' Get Article Links
#'
#' Extracts a list of article links from an XML or HTML sitemap of a day.
#' @param day_link A character string representing the URL of a link containing article links.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of the article links.
#' @import httr
#' @export
gu_article_links <- function(day_link, tag_type = NULL, tag_class = NULL) {
  # Fetch the content of the URL
  response <- tryCatch(GET(day_link), error = function(e) {
    message("Error fetching URL: ", e)
    return(NULL)
  })

  # Check if the request was successful
  if (is.null(response) || http_status(response)$category != "Success") {
    message("Failed to retrieve the URL content.")
    return(NULL)
  }

  # Determine the content type and get the content text
  content_type <- headers(response)$`content-type`
  content_text <- content(response, as = "text")

  # Use the helper function to parse the sitemap content
  all_links <- gu_parse_sitemap(content_text = content_text,
                                content_type = content_type,
                                tag_type = tag_type,
                                tag_class = tag_class)

  # Return NULL if parsing failed
  if (is.null(all_links)) return(NULL)

  return(all_links)
}


#' Get Month Links for a Specific Year
#'
#' Retrieves and filters month links for a given year based on a specified range.
#' @param year_link A character string representing the URL of the year link.
#' @param year The year as a numeric value.
#' @param year_min The starting year of the range.
#' @param year_max The ending year of the range.
#' @param start_date The start date of the entire range.
#' @param end_date The end date of the entire range.
#' @param month_tag_type A character string for the month tag type (e.g., "ol").
#' @param month_tag_class A character string for the month tag class.
#' @param website_structure The base structure of the website for appending links.
#' @return A character vector of filtered month links.
#' @export
gu_apply_month_links <- function(year_link, year, year_min, year_max, start_date, end_date,
                               month_tag_type, month_tag_class, website_structure) {
  # Set month range for the current year
  if (year == year_min && year == year_max) {
    month_min <- as.numeric(format(start_date, "%m"))
    month_max <- as.numeric(format(end_date, "%m"))
  } else if (year == year_min) {
    month_min <- as.numeric(format(start_date, "%m"))
    month_max <- 12
  } else if (year == year_max) {
    month_min <- 1
    month_max <- as.numeric(format(end_date, "%m"))
  } else {
    month_min <- 1
    month_max <- 12
  }

  message("Getting month links for year ", year, " with the following parameters:")
  message("Month range: ", month_min, " to ", month_max)
  message("Year link: ", year_link)
  message("Tag type: ", month_tag_type)
  message("Tag class: ", month_tag_class)

  # Get month links using `gu_month_links`
  month_links <- gu_month_links(
    year_link,
    month_min = month_min,
    month_max = month_max,
    tag_type = month_tag_type,
    tag_class = month_tag_class
  )

  message("Filtered month links for year ", year, ":")
  print(month_links)

  # Check if need to use gu_append_links for month links
  if (any(!grepl(website_structure, month_links))) {
    message("Appending base URL to month links...")
    month_links <- gu_append_links(year_link, month_links)
  }

  return(month_links)
}


#' Get Day Links for a Specific Month
#'
#' Retrieves and filters day links for a given month based on a specified range.
#' @param month_link A character string representing the URL of the month link.
#' @param month The month as a numeric value.
#' @param year The year as a numeric value.
#' @param year_min The starting year of the range.
#' @param year_max The ending year of the range.
#' @param start_date The start date of the entire range.
#' @param end_date The end date of the entire range.
#' @param day_tag_type A character string for the day tag type (e.g., "ol").
#' @param day_tag_class A character string for the day tag class.
#' @param website_structure The base structure of the website for appending links.
#' @return A character vector of filtered day links.
#' @export
gu_apply_day_links <- function(month_link, month, year, year_min, year_max, start_date, end_date,
                               day_tag_type, day_tag_class, website_structure) {
  # Set day range for the current month
  if (year == year_min && month == as.numeric(format(start_date, "%m")) &&
      year == year_max && month == as.numeric(format(end_date, "%m"))) {
    day_min <- as.numeric(format(start_date, "%d"))
    day_max <- as.numeric(format(end_date, "%d"))
  } else if (year == year_min && month == as.numeric(format(start_date, "%m"))) {
    day_min <- as.numeric(format(start_date, "%d"))
    day_max <- 31
  } else if (year == year_max && month == as.numeric(format(end_date, "%m"))) {
    day_min <- 1
    day_max <- as.numeric(format(end_date, "%d"))
  } else {
    day_min <- 1
    day_max <- 31
  }

  message("Getting day links for month ", month, " with the following parameters:")
  message("Day range: ", day_min, " to ", day_max)
  message("Month link: ", month_link)
  message("Tag type: ", day_tag_type)
  message("Tag class: ", day_tag_class)

  # Get day links using `gu_day_links`
  day_links <- gu_day_links(
    month_link,
    day_min = day_min,
    day_max = day_max,
    tag_type = day_tag_type,
    tag_class = day_tag_class
  )

  message("Filtered day links for month ", month, ":")
  print(day_links)

  # Check if need to use gu_append_links for day links
  if (any(!grepl(website_structure, day_links))) {
    message("Appending base URL to day links...")
    day_links <- gu_append_links(month_link, day_links)
  }

  return(day_links)
}


#' Get All Article Links from a List of Day Links
#'
#' This function calls `gu_article_links` for each link in a list of day URLs and returns all article links.
#' @param day_links A character vector containing the list of day URLs.
#' @param tag_type An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".
#' @param tag_class An optional character string specifying the class of the tag containing links.
#' @return A list of URLs of all the article links.
#' @import purrr
#' @export
gu_apply_article_links <- function(day_links, tag_type = NULL, tag_class = NULL) {
  # Use purrr::map to call gu_article_links for each day link
  all_article_links <- purrr::map(day_links, ~ {
    gu_article_links(day_link = .x, tag_type = tag_type, tag_class = tag_class)
  })

  # Flatten the list of lists into a single vector of article links
  all_article_links <- purrr::flatten_chr(all_article_links)

  return(all_article_links)
}


# Main Functions ----------------------------------------------------------

# FIXME HOT, FLAMING garbage

#' Get Links from a Website
#'
#' This function retrieves the list of article links from a given website and date range using schema details.
#' If multiple schema rows are returned from `gs_pull_schema`, it issues a warning to specify the `id` parameter.
#' @param website_url A character string representing the URL of the website (e.g., "https://www.nytimes.com").
#' @param start_date The start date for retrieving article links (e.g., "2003-03-11").
#' @param end_date The end date for retrieving article links (e.g., "2004-12-31").
#' @param id A character string specifying the unique ID of the schema to pull (optional).
#' @return A character vector of article links.
#' @importFrom stringr str_extract
#' @export
gu_get_links <- function(website_url, start_date, end_date, id = NULL) {
  # Convert start and end dates to Date class
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Extract the year ranges from the start and end dates
  year_min <- as.numeric(format(start_date, "%Y"))
  year_max <- as.numeric(format(end_date, "%Y"))

  # Pull the schema for the website
  message("Fetching schema for the website...")
  schema <- gs_pull_schema(website_url, id)

  # Check if multiple rows were returned
  if (nrow(schema) > 1) {
    warning("Multiple schema entries found. Please specify the `id` parameter.")
    return(NULL)
  }

  # Extract necessary schema information
  tag_type <- schema$year_type
  tag_class <- schema$year_class
  month_tag_type <- schema$month_type
  month_tag_class <- schema$month_class
  day_tag_type <- schema$day_type
  day_tag_class <- schema$day_class
  article_tag_type <- schema$article_type
  article_tag_class <- schema$article_class
  website_structure <- schema$website_structure

  # Get year links
  message("Getting year links...")
  year_links <- gu_year_links(website_url, year_min, year_max, tag_type = tag_type, tag_class = tag_class)
  message("Year links found:")
  print(year_links)

  # Check if need to use gu_append_links
  if (any(!grepl(website_structure, year_links))) {
    message("Appending base URL to year links...")
    year_links <- gu_append_links(website_url, year_links)
    message("Year links after appending:")
    print(year_links)
  }

  # Initialize vectors to store month and day links
  all_month_links <- character()
  all_day_links <- character()

  # Loop through each year link to get month and day links
  for (year_link in year_links) {
    # Extract the year from the year link
    year <- as.numeric(str_extract(year_link, "\\d{4}"))
    if (is.na(year)) {
      message("Skipping year link due to invalid year extraction: ", year_link)
      next
    }

    # Get month links using the helper function
    month_links <- gu_apply_month_links(
      year_link = year_link,
      year = year,
      year_min = year_min,
      year_max = year_max,
      start_date = start_date,
      end_date = end_date,
      month_tag_type = month_tag_type,
      month_tag_class = month_tag_class,
      website_structure = website_structure
    )
    all_month_links <- c(all_month_links, month_links)

    # Loop through each month link to get day links using the helper function
    for (month_link in month_links) {
      # Corrected regex pattern for extracting the month
      month <- as.numeric(str_extract(month_link, "(?<=/)(\\d{2})(?=/)"))
      if (is.na(month)) {
        message("Skipping month link due to invalid month extraction: ", month_link)
        next
      }

      # Get day links using the helper function
      day_links <- gu_apply_day_links(
        month_link = month_link,
        month = month,
        year = year,
        year_min = year_min,
        year_max = year_max,
        start_date = start_date,
        end_date = end_date,
        day_tag_type = day_tag_type,
        day_tag_class = day_tag_class,
        website_structure = website_structure
      )
      all_day_links <- c(all_day_links, day_links)
    }
  }

  message("All filtered month links:")
  print(all_month_links)
  message("All filtered day links:")
  print(all_day_links)

  # Get article links from day links
  message("Getting article links from day links...")
  article_links <- gu_apply_article_links(
    day_links = all_day_links,
    tag_type = article_tag_type,
    tag_class = article_tag_class
  )

  message("Article link extraction completed.")
  return(article_links)
}

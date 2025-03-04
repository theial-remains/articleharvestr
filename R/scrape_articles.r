# it does what it says on the tin

#' Scrape Entire Article Content
#'
#' This function retrieves the raw HTML content of an article from a given URL.
#'
#' @param article_url A character string representing the URL of the article to scrape.
#' @return An rvest HTML document object containing the full article content.
#' @import rvest
#' @import httr
#' @export
sa_get_html <- function(article_url) {
  tryCatch({
    response <- GET(article_url)

    # check if the URL is accessible
    if (http_error(response)) {
      message("Error: Unable to access URL - ", article_url)
      return(NULL)
    }

    webpage <- read_html(response)
    return(webpage)
  }, error = function(e) {
    message("Error retrieving article: ", article_url, " - ", e$message)
    return(NULL)
  })
}

#' Extract Article Title
#' @param article_html An rvest HTML document object.
#' @param url The article URL.
#' @return A character string representing the article title or NA if not found.
#' @import rvest
#' @export
sa_extract_title <- function(article_html, url) {
  if (is.null(article_html)) return(NA)

  # get custom selectors
  selectors <- sa_get_selectors(url)

  if (!is.null(selectors$title_selector)) {
    title_text <- article_html %>%
      html_node(selectors$title_selector) %>%
      html_text(trim = TRUE)

    if (!is.na(title_text) && title_text != "") {
      return(title_text)
    }
  }

  # fallback logic
  title_text <- article_html %>%
    html_node("title") %>%
    html_text(trim = TRUE)

  if (is.na(title_text) || title_text == "") {
    headline_text <- article_html %>%
      html_nodes(xpath = "//*[contains(@class, 'headline') or contains(@id, 'headline')]") %>%
      html_text(trim = TRUE) %>%
      na.omit()

    if (length(headline_text) > 0) return(headline_text[1])
  }

  return(ifelse(title_text == "", NA, title_text))
}

#' Extract Article Author
#' @param article_html An rvest HTML document object.
#' @param url The article URL.
#' @return A character string representing the author's name or NA if not found.
#' @import rvest
#' @export
sa_extract_author <- function(article_html, url) {
  if (is.null(article_html)) return(NA)

  # get custom selectors
  selectors <- sa_get_selectors(url)

  if (!is.null(selectors$author_selector)) {
    author_text <- article_html %>%
      html_node(selectors$author_selector) %>%
      html_text(trim = TRUE)

    if (!is.na(author_text) && author_text != "") {
      return(author_text)
    }
  }

  # fallback logic
  author_text <- article_html %>%
    html_node("meta[name='author'], meta[property='article:author'], meta[property='og:author']") %>%
    html_attr("content")

  if (is.na(author_text) || author_text == "") {
    byline_text <- article_html %>%
      html_nodes(xpath = "//*[contains(@class, 'byline') or contains(@id, 'byline')]") %>%
      html_text(trim = TRUE) %>%
      na.omit()

    if (length(byline_text) > 0) return(byline_text[1])
  }

  return(ifelse(author_text == "", NA, author_text))
}

#' Extract Published Date
#' @param article_html An rvest HTML document object.
#' @param url The article URL.
#' @return A character string representing the published date or NA if not found.
#' @import rvest
#' @export
sa_extract_date <- function(article_html, url) {
  if (is.null(article_html)) return(NA)

  # get custom selectors
  selectors <- sa_get_selectors(url)

  if (!is.null(selectors$date_selector)) {
    published_date <- article_html %>%
      html_node(selectors$date_selector) %>%
      html_text(trim = TRUE)

    if (!is.na(published_date) && published_date != "") {
      return(published_date)
    }
  }

  # fallback logic
  published_date <- article_html %>%
    html_node("time") %>%
    html_text(trim = TRUE)

  if (is.na(published_date) || published_date == "") {
    published_date <- article_html %>%
      html_node("meta[property='article:published_time'], meta[property='datePublished'], meta[name='date']") %>%
      html_attr("content")
  }

  return(ifelse(published_date == "", NA, published_date))
}

#' Extract Article Text
#' @param article_html An rvest HTML document object.
#' @param url The article URL.
#' @return A character string representing the cleaned article text or NA if not found.
#' @import rvest
#' @export
sa_extract_text <- function(article_html, url) {
  if (is.null(article_html)) return(NA)

  # get custom selectors
  selectors <- sa_get_selectors(url)

  if (!is.null(selectors$text_selector)) {
    article_text <- article_html %>%
      html_nodes(selectors$text_selector) %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")

    if (!is.na(article_text) && article_text != "") {
      return(article_text)
    }
  }

  # fallback logic
  all_paragraphs <- article_html %>%
    html_nodes('p') %>%
    html_text(trim = TRUE)

  excluded_paragraphs <- article_html %>%
    html_nodes('#support-huffpost-entry p, .footer p, .advertisement p') %>%
    html_text(trim = TRUE)

  article_text <- setdiff(all_paragraphs, excluded_paragraphs) %>%
    paste(collapse = " ")

  return(ifelse(article_text == "", NA, article_text))
}

#' Scrape and Extract Full Article Data
#'
#' @param article_url A character string representing the URL of the article.
#' @return A data frame containing the article data.
#' @import dplyr
#' @export
sa_get_article_data <- function(article_url) {
  article_html <- sa_get_html(article_url)

  if (is.null(article_html)) {
    message("Failed to retrieve article: ", article_url)
    return(data.frame(url = article_url, published_date = NA, author = NA, title = NA, text = NA, stringsAsFactors = FALSE))
  }

  df <- data.frame(
    url = article_url,
    published_date = sa_extract_date(article_html),
    author = sa_extract_author(article_html),
    title = sa_extract_title(article_html),
    text = sa_extract_text(article_html),
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Scrape Multiple Articles
#'
#' @param article_urls A character vector containing multiple article URLs.
#' @return A data frame where each row represents an article.
#' @import dplyr
#' @import purrr
#' @export
sa_scrape_articles <- function(article_urls) {
  if (length(article_urls) == 0) {
    stop("No URLs provided.")
  }

  articles_df <- map_dfr(article_urls, function(url) {
    message("Scraping: ", url)
    sa_get_article_data(url)
  })

  return(articles_df)
}

#' Scrape and Sample Articles by Month
#'
#' This function extracts article URLs for a given year and month range,
#' randomly samples 100 articles per month, and returns a final list.
#'
#' @param sitemap_url The base sitemap URL.
#' @param year The year to scrape articles from.
#' @param month_start The starting month (e.g., 1 for January).
#' @param month_end The ending month (e.g., 12 for December).
#' @return A character vector of sampled article URLs.
#' @import dplyr
#' @import stringr
#' @import purrr
#' @export
sa_sample_article_urls <- function(sitemap_url,
                                   year,
                                   month_start,
                                   month_end) {
  final_article_urls <- character(0)

  for (month in month_start:month_end) {
    month_str <- sprintf("%02d", month)
    last_day <- as.character(as.Date(paste0(year, "-", month_str, "-01")) + 31)
    last_day <- format(as.Date(last_day) - as.numeric(format(as.Date(last_day), "%d")), "%d")
    start_date <- paste0(year, "-", month_str, "-01")
    end_date <- paste0(year, "-", month_str, "-", last_day)  # because months have different amounts of days

    message("Fetching articles for ", start_date, " to ", end_date, "...")
    article_urls <- gu_fetch_sitemap_articles(sitemap_url, levels = 1, start_date, end_date)

    if (length(article_urls) == 0) {
      message("No articles found for ", start_date, " to ", end_date)
      next  # skip to the next month
    }

    sampled_articles <- sample(article_urls, min(100, length(article_urls)))
    final_article_urls <- c(final_article_urls, sampled_articles)
    message("Sampled ", length(sampled_articles),
            " articles for ", month_str, " ", year)
  }

  message("Completed scraping. Returning ", length(final_article_urls),
          " sampled articles.")

  return(final_article_urls)
}

#' Load Selectors for a Given News Website
#'
#' This function retrieves the appropriate CSS/XPath selectors for extracting article elements.
#' @param url A character string representing the article URL.
#' @return A named list containing the CSS/XPath selectors for title, author, date, and text.
#' @import readr
#' @export
sa_get_selectors <- function(url) {
  # Load selectors dataframe
  selectors_df <- read.csv(system.file("extdata", "news_selectors.csv", package = "yourPackageName"),
                           stringsAsFactors = FALSE)

  # Extract domain name from URL
  domain <- gsub("https?://(www\\.)?", "", url)
  domain <- gsub("/.*", "", domain)  # Keep only the base domain

  # Filter for the matching website
  site_selectors <- selectors_df[grepl(domain, selectors_df$website), ]

  if (nrow(site_selectors) == 0) {
    message("No custom selectors found for: ", domain)
    return(NULL)
  }

  return(list(
    title_selector = site_selectors$title_selector,
    author_selector = site_selectors$author_selector,
    date_selector = site_selectors$date_selector,
    text_selector = site_selectors$text_selector
  ))
}

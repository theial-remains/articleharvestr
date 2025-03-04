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
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the title.
#' @return A character string representing the article title or NA if not found.
#' @import rvest
#' @export
sa_extract_title <- function(article_html, selector) {
  if (is.null(article_html)) return(NA)

  # try selector
  if (!is.null(selector) && selector != "") {
    title_text <- article_html %>%
      html_node(selector) %>%
      html_text(trim = TRUE)
    if (!is.na(title_text) && title_text != "") return(title_text)
  }

  # fallback
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
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the author.
#' @return A character string representing the author's name or NA if not found.
#' @import rvest
#' @export
sa_extract_author <- function(article_html, selector) {
  if (is.null(article_html)) return(NA)

  # try selector
  if (!is.null(selector) && selector != "") {
    author_text <- article_html %>%
      html_node(selector) %>%
      html_text(trim = TRUE)
    if (!is.na(author_text) && author_text != "") return(author_text)
  }

  # fallback
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
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the date.
#' @return A character string representing the published date or NA if not found.
#' @import rvest
#' @export
sa_extract_date <- function(article_html, selector) {
  if (is.null(article_html)) return(NA)

  # try selector
  if (!is.null(selector) && selector != "") {
    published_date <- article_html %>%
      html_node(selector) %>%
      html_text(trim = TRUE)
    if (!is.na(published_date) && published_date != "") return(published_date)
  }

  # fallback
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
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the article text.
#' @return A character string representing the cleaned article text or NA if not found.
#' @import rvest
#' @export
sa_extract_text <- function(article_html, selector) {
  if (is.null(article_html)) return(NA)

  # try selector
  if (!is.null(selector) && selector != "") {
    article_text <- article_html %>%
      html_nodes(selector) %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    if (!is.na(article_text) && article_text != "") return(article_text)
  }

  # fallback
  all_paragraphs <- article_html %>%
    html_nodes('p') %>%
    html_text(trim = TRUE)

  return(ifelse(length(all_paragraphs) == 0,
                NA,
                paste(all_paragraphs, collapse = " ")))
}

#' Scrape and Extract Full Article Data
#'
#' @param article_url A character string representing the URL of the article.
#' @param selectors A named list of CSS/XPath selectors (title, author, date, text), or NULL if not found.
#' @return A data frame containing the article data.
#' @import dplyr
#' @export
sa_get_article_data <- function(article_url, selectors = NULL) {
  article_html <- sa_get_html(article_url)

  if (is.null(article_html)) {
    message("Failed to retrieve article: ", article_url)
    return(data.frame(
      url = article_url,
      published_date = NA,
      author = NA,
      title = NA,
      text = NA,
      stringsAsFactors = FALSE
    ))
  }

  # NULL passed if selectors are missing
  selectors <- selectors %||% list(title_selector = NULL,
                                   author_selector = NULL,
                                   date_selector = NULL,
                                   text_selector = NULL)

  df <- data.frame(
    url = article_url,
    published_date = sa_extract_date(article_html, selectors$date_selector),
    author = sa_extract_author(article_html, selectors$author_selector),
    title = sa_extract_title(article_html, selectors$title_selector),
    text = sa_extract_text(article_html, selectors$text_selector),
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Load Selectors for a Given News Website
#'
#' @param url A character string representing the article URL.
#' @return A named list containing the CSS/XPath selectors for title, author, date, and text, or NULL if no custom selectors exist.
#' @import readr
#' @export
sa_get_selectors <- function(url) {
  file_path <- system.file("extdata", "news_selectors.csv", package = "yourPackageName")

  if (file_path == "") {
    message("Development mode detected: Using local file path.")
    file_path <- "inst/extdata/news_selectors.csv"
  }

  if (!file.exists(file_path)) {
    stop("Error: Selectors CSV file not found at ", file_path)
  }

  selectors_df <- read.csv(file_path, stringsAsFactors = FALSE)

  # Standardize domain: Remove "www." and keep only base domain
  domain <- gsub("https?://(www\\.)?", "", url)
  domain <- gsub("/.*", "", domain)

  site_selectors <- selectors_df[grepl(domain, selectors_df$website, ignore.case = TRUE), ]

  if (nrow(site_selectors) == 0) {
    message("No custom selectors found for: ", domain)
    return(NULL)  # Return NULL so fallback methods are used
  }

  return(list(
    title_selector = ifelse(nzchar(site_selectors$title_selector[1]), site_selectors$title_selector[1], NULL),
    author_selector = ifelse(nzchar(site_selectors$author_selector[1]), site_selectors$author_selector[1], NULL),
    date_selector = ifelse(nzchar(site_selectors$date_selector[1]), site_selectors$date_selector[1], NULL),
    text_selector = ifelse(nzchar(site_selectors$text_selector[1]), site_selectors$text_selector[1], NULL)
  ))
}

#' Scrape Multiple Articles Efficiently
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

  domains <- unique(stringr::str_extract(article_urls, "(?<=://)([^/]+)"))
  domains <- gsub("^www\\.", "", domains)

  domain_selectors <- setNames(
    map(domains, function(domain) sa_get_selectors(domain) %||% NULL),
    domains
  )

  articles_df <- map_dfr(article_urls, function(url) {
    message("Scraping: ", url)
    domain <- stringr::str_extract(url, "(?<=://)([^/]+)")
    selectors <- domain_selectors[[domain]]

    sa_get_article_data(url, selectors)
  })

  return(articles_df)
}

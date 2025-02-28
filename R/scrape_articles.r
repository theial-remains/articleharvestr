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
#' @return A character string representing the article title or NA if not found.
#' @import rvest
#' @export
sa_extract_title <- function(article_html) {
  if (is.null(article_html)) return(NA)

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
#' @return A character string representing the author's name or NA if not found.
#' @import rvest
#' @export
sa_extract_author <- function(article_html) {
  if (is.null(article_html)) return(NA)

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
#' @return A character string representing the published date or NA if not found.
#' @import rvest
#' @export
sa_extract_date <- function(article_html) {
  if (is.null(article_html)) return(NA)

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
#' @return A character string representing the cleaned article text or NA if not found.
#' @import rvest
#' @export
sa_extract_text <- function(article_html) {
  if (is.null(article_html)) return(NA)

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

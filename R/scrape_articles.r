# it does what it says on the tin

#' Scrape Entire Article Content
#'
#' This function retrieves the raw HTML content of an article from a given URL.
#'
#' @param article_url A character string representing the URL of the article to scrape.
#' @return An rvest HTML document object containing the full article content.
#' @import rvest
#' @export
sa_get_html <- function(article_url) {
  tryCatch({
    webpage <- read_html(article_url)
    return(webpage)
  }, error = function(e) {
    message("Error retrieving article: ", article_url, " - ", e)
    return(NULL)
  })
}

#' Extract Article Title
#'
#' This function extracts the title from a scraped article HTML document.
#'
#' @param article_html An rvest HTML document object containing the full article content.
#' @return A character string representing the article title.
#' @import rvest
#' @export
sa_extract_title <- function(article_html) {
  if (is.null(article_html)) return(NA)

  # extract from any element that contains "headline" in its class, id, tag, or attributes
  title_text <- article_html %>%
    html_node("title") %>%
    html_text(trim = TRUE)

    if (is.na(title_text) || title_text == "") {
    headline_text <- article_html %>%
      html_nodes(xpath = "//*[contains(@class, 'headline') or
                           contains(@id, 'headline') or
                           contains(name(), 'headline') or
                           contains(@*, 'headline')]") %>%
      html_text(trim = TRUE) %>%
      na.omit()

    # Return first valid headline if available
    if (length(headline_text) > 0) {
      return(headline_text[1])
    }
  }

  return(title_text)
}

#' Extract Article Author
#'
#' This function extracts the author's name from a scraped article HTML document.
#'
#' @param article_html An rvest HTML document object containing the full article content.
#' @return A character string representing the author's name.
#' @import rvest
#' @importFrom stringr str_replace
#' @export
sa_extract_author <- function(article_html) {
  if (is.null(article_html)) return(NA)

  # from <meta> tags
  author_text <- article_html %>%
    html_node("meta[name='author'], meta[property='article:author'], meta[property='og:author']") %>%
    html_attr("content")

  # if meta NA, look for byline in class, ID, or attr.
  if (is.na(author_text) || author_text == "") {
    byline_text <- article_html %>%
      html_nodes(xpath = "//*[contains(@class, 'byline') or
                           contains(@id, 'byline') or
                           contains(name(), 'byline')]") %>%
      html_text(trim = TRUE) %>%
      na.omit()

    if (length(byline_text) > 0) {
      return(byline_text[1]) # first instance returned
    }
  }

  return(author_text)
}


#' Extract Published Date
#'
#' This function extracts the published date from a scraped article HTML document.
#'
#' @param article_html An rvest HTML document object containing the full article content.
#' @return A character string representing the published date.
#' @import rvest
#' @export
sa_extract_date <- function(article_html) {
  if (is.null(article_html)) return(NA)

  published_date <- article_html %>%
      html_node("time") %>%
      html_text(trim = TRUE)

  # try meta tag
  if (is.na(published_date) || published_date == "") {
    published_date <- article_html %>%
      html_node("meta[property='article:published_time']") %>%
      html_attr("content")
  }

  return(published_date)
}

#' Extract Article Text
#'
#' This function extracts the article text, filtering out unwanted content.
#'
#' @param article_html An rvest HTML document object containing the full article content.
#' @return A character string representing the cleaned article text.
#' @import rvest
#' @export
sa_extract_text <- function(article_html) {
  if (is.null(article_html)) return(NA)

  all_paragraphs <- article_html %>%
    html_nodes('p') %>%
    html_text(trim = TRUE)

  # Remove known irrelevant paragraphs (e.g., disclaimers, footers, support messages)
  excluded_paragraphs <- article_html %>%
    html_nodes('#support-huffpost-entry p, .footer p, .advertisement p') %>%
    html_text(trim = TRUE)

  article_text <- setdiff(all_paragraphs, excluded_paragraphs) %>%
    paste(collapse = " ")

  return(article_text)
}

#' Scrape and Extract Full Article Data
#'
#' This function scrapes an article and extracts its key components, then organizes them into a dataframe.
#'
#' @param article_url A character string representing the URL of the article to scrape.
#' @return A data frame containing the article URL, title, author, published date, and text.
#' @import dplyr
#' @export
sa_get_article_data <- function(article_url) {
  article_html <- sa_get_html(article_url)

  if (is.null(article_html)) {
    message("Failed to retrieve article: ", article_url)
    return(NA)
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


#' Scrape Multiple Articles from DataFrame with 404 Prevention
#'
#' This function maps the `sa_scrape_article_data` function over the "url" column
#' of a given dataframe, limiting the number of requests and adding polite delays
#' to prevent 404 errors or overloading the server. The results are returned as a
#' combined data frame with the scraped data for all articles and the corresponding URLs.
#'
#' @param articles_df A data frame containing a column named "url" with article URLs to scrape.
#' @param max_requests An integer. Maximum number of requests to process in one session (default: 15).
#' @param delay An integer. Number of seconds to delay between each request (default: 5).
#' @return A data frame containing the scraped data for all articles, with columns: url, title, author, published_date, and article_text.
#' @importFrom purrr map_dfr
#' @importFrom utils head
#' @export
sa_scrape_articles <- function(articles_df, max_requests = 15, delay = 5) {
  if (!"url" %in% names(articles_df)) {
    stop("Input dataframe must contain a 'url' column.")
  }

  # Extract URLs from the "url" column
  article_urls <- articles_df$url

  # Limit the number of requests to `max_requests`
  article_urls <- head(article_urls, max_requests)

  # Scrape each article with polite delay
  scraped_data <- purrr::map_dfr(article_urls, function(url) {
    tryCatch(
      {
        message("Processing URL: ", url)
        Sys.sleep(delay)  # Be polite to the server
        article_data <- sa_scrape_article_data(url)
        # Add the URL to the scraped data
        article_data$url <- url
        article_data
      },
      error = function(e) {
        message("Error scraping article: ", url, " - ", e)
        # Return a row with NA values and the URL for failed articles
        data.frame(
          url = url,
          title = NA,
          author = NA,
          published_date = NA,
          article_text = NA,
          stringsAsFactors = FALSE
        )
      }
    )
  })

  return(scraped_data)
}

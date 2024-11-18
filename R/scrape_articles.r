# it does what it says on the tin


#' Scrape Article Data
#'
#' This function scrapes key data (title, author, published date, and article text) from a single article URL.
#'
#' @param article_url A character string representing the URL of the article to scrape.
#' @return A data frame containing the scraped data: title, author, published date, and article text.
#' @import rvest
#' @importFrom stringr str_replace
#' @export
sa_scrape_article_data <- function(article_url) {
  webpage <- read_html(article_url)

  # Extract title
  title <- webpage %>%
    html_node("title") %>%
    html_text(trim = TRUE)

  # Extract all paragraphs and exclude irrelevant ones
  all_paragraphs <- webpage %>%
    html_nodes('.primary-cli.cli.cli-text p') %>%
    html_text(trim = TRUE)

  excluded_paragraphs <- webpage %>%
    html_nodes('#support-huffpost-entry p') %>%
    html_text(trim = TRUE)

  article_text <- setdiff(all_paragraphs, excluded_paragraphs) %>%
    paste(collapse = " ")

  # Extract published date
  published_date <- webpage %>%
    html_node("meta[property='article:published_time']") %>%
    html_attr("content")

  # Extract author name
  author <- webpage %>%
    html_node(".author-list a") %>%
    html_attr("aria-label") %>%
    str_replace("By ", "")

  # Create data frame with scraped information
  df <- data.frame(
    Title = title,
    Author = author,
    Published_Date = published_date,
    Article_Text = article_text,
    stringsAsFactors = FALSE
  )

  return(df)
}


#' Scrape Multiple Articles from DataFrame with 404 Prevention
#'
#' This function maps the `sa_scrape_article_data` function over the "url" column
#' of a given dataframe, limiting the number of requests and adding polite delays
#' to prevent 404 errors or overloading the server. The results are returned as a
#' combined data frame with the scraped data for all articles.
#'
#' @param articles_df A data frame containing a column named "url" with article URLs to scrape.
#' @param max_requests An integer. Maximum number of requests to process in one session (default: 15).
#' @param delay An integer. Number of seconds to delay between each request (default: 5).
#' @return A data frame containing the scraped data for all articles, with columns: title, author, published date, and article text.
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
        sa_scrape_article_data(url)
      },
      error = function(e) {
        message("Error scraping article: ", url, " - ", e)
        # Return an empty data frame for failed articles
        data.frame(
          Title = NA,
          Author = NA,
          Published_Date = NA,
          Article_Text = NA,
          stringsAsFactors = FALSE
        )
      }
    )
  })

  return(scraped_data)
}

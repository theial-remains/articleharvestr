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

#' Load Selectors for a Given News Website
#'
#' @param url A character string representing the article URL.
#' @return A named list containing the CSS/XPath selectors and tags or NULL if no custom selectors exist.
#' @import readr
#' @export
sa_get_selectors <- function(url) {
  file_path <- system.file("extdata", "news_selectors.csv", package = "articleharvestr")

  if (file_path == "") {
    message("Development mode detected: Using local file path.")
    file_path <- "inst/extdata/news_selectors.csv"
  }

  if (!file.exists(file_path)) {
    stop("Error: Selectors CSV file not found at ", file_path)
  }

  selectors_df <- read.csv(file_path, stringsAsFactors = FALSE)

  domain <- gsub("https?://(www\\.)?", "", url)
  domain <- gsub("/.*", "", domain)

  site_selectors <- selectors_df[grepl(domain, selectors_df$website, ignore.case = TRUE), ]

  if (nrow(site_selectors) == 0) {
    message("No custom selectors found for: ", domain)
    return(NULL)
  }

  return(list(
    title_element = site_selectors$title_element[1],
    title_tag = site_selectors$title_tag[1],
    author_element = site_selectors$author_element[1],
    author_tag = site_selectors$author_tag[1],
    date_element = site_selectors$date_element[1],
    date_tag = site_selectors$date_tag[1],
    text_element = site_selectors$text_element[1],
    text_tag = site_selectors$text_tag[1]
  ))
}

#' Extract Article Author
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the author.
#' @param tag The CSS tag
#' @return A character string representing the author's name or NA if not found.
#' @import rvest
#' @export
sa_extract_author <- function(article_html, selector = NULL, tag = NULL) {
  if (is.null(article_html)) return(NA)

  # selector
  if (!is.null(selector) && selector != "") {
    author_node <- article_html %>% html_node(selector)

    if (!is.null(author_node)) {
      author_text <- if (!is.null(tag) && tag == "text") {
        author_node %>% html_text(trim = TRUE)
      } else {
        author_node %>% html_attr(tag)
      }
      if (!is.na(author_text) && author_text != "") {
        return(author_text)
      }
    }
  }

  # fallback
  author_text <- article_html %>%
    html_node("meta[name='author'], meta[property='article:author'], meta[property='og:author']") %>%
    html_attr("content")

  if (!is.na(author_text) && author_text != "") {
    return(author_text)
  }

  byline_text <- article_html %>%
    html_nodes(xpath = "//*[contains(@class, 'byline') or contains(@id, 'byline') or contains(@class, 'byline_name')]") %>%
    html_text(trim = TRUE) %>%
    na.omit()

  if (length(byline_text) > 0) {
    return(byline_text[1])
  }

  return(NA)
}

#' Extract Published Date
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the date.
#' @param tag The CSS tag
#' @return A character string representing the published date or NA if not found.
#' @import rvest
#' @export
sa_extract_date <- function(article_html, selector = NULL, tag = NULL) {
  if (is.null(article_html)) return(NA)

  # selector
  if (!is.null(selector) && selector != "") {
    date_node <- article_html %>% html_node(selector)

    if (!is.null(date_node)) {
      date_text <- if (tag == "text") {
        date_node %>% html_text(trim = TRUE)
      } else {
        date_node %>% html_attr(tag)
      }

      if (!is.na(date_text) && date_text != "") {
        return(date_text)
      }
    }
  }

  # fallback
  date_text <- article_html %>%
    html_node("time") %>%
    html_text(trim = TRUE)

  if (!is.na(date_text) && date_text != "") {
    return(date_text)
  }

  date_text <- article_html %>%
    html_node("meta[property='article:published_time'], meta[property='datePublished'], meta[name='date']") %>%
    html_attr("content")

  if (!is.na(date_text) && date_text != "") {
    return(date_text)
  }

  return(NA)
}

#' Extract Article Title
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the title.
#' @param tag The CSS tag
#' @return A character string representing the article title or NA if not found.
#' @import rvest
#' @export
sa_extract_title <- function(article_html, selector = NULL, tag = NULL) {
  if (is.null(article_html) || is.null(selector) || selector == "") return(NA)

  # selector
  if (!is.null(selector) && selector != "") {
    title_node <- article_html %>% html_node(selector)

    if (!is.null(title_node)) {
      title_text <- if (tag == "text") {
        title_node %>% html_text(trim = TRUE)
      } else {
        title_node %>% html_node(tag) %>% html_text(trim = TRUE)
      }

      if (!is.na(title_text) && title_text != "") {
        return(title_text)
      }
    }
  }

  # fallback
  title_text <- article_html %>%
    html_node("title") %>%
    html_text(trim = TRUE)

  if (!is.na(title_text) && title_text != "") {
    return(title_text)
  }

  headline_text <- article_html %>%
    html_nodes(xpath = "//*[contains(@class, 'headline') or contains(@id, 'headline')]") %>%
    html_text(trim = TRUE) %>%
    na.omit()

  if (length(headline_text) > 0) {
    return(headline_text[1])
  }

  return(NA)
}

#' Extract Article Text
#'
#' @param article_html An rvest HTML document object.
#' @param selector The CSS/XPath selector for extracting the article text.
#' @param tag The CSS tag
#' @return A character string representing the cleaned article text or NA if not found.
#' @import rvest
#' @export
sa_extract_text <- function(article_html, selector = NULL, tag = NULL) {
  if (is.null(article_html) || is.null(selector) || selector == "") return(NA)

  # selector
  if (!is.null(selector) && selector != "") {
    text_nodes <- article_html %>% html_nodes(selector)

    if (length(text_nodes) > 0) {
      article_text <- if (tag == "text") {
        text_nodes %>% html_text(trim = TRUE) %>% paste(collapse = " ")
      } else {
        text_nodes %>% html_nodes(tag) %>% html_text(trim = TRUE) %>% paste(collapse = " ")
      }

      if (!is.na(article_text) && article_text != "") {
        return(article_text)
      }
    }
  }

  # fallback
  all_paragraphs <- article_html %>%
    html_nodes('p') %>%
    html_text(trim = TRUE)

  if (length(all_paragraphs) > 0) {
    return(paste(all_paragraphs, collapse = " "))
  }

  return(NA)
}

#' Scrape and Extract Full Article Data
#'
#' @param article_url A character string representing the URL of the article.
#' @param selectors A named list of CSS/XPath selectors and tags (can be NULL).
#' @return A data frame containing the article data.
#' @import dplyr
#' @export
sa_get_article_data <- function(article_url, selectors = NULL) {
  article_html <- sa_get_html(article_url)

  if (is.null(article_html)) {
    return(data.frame(
      url = article_url,
      published_date = NA,
      author = NA,
      title = NA,
      text = NA,
      stringsAsFactors = FALSE
    ))
  }

  # make sure selectors exist, otherwise default to NULL for fallback
  title_element <- if (!is.null(selectors)) selectors$title_element else NULL
  title_tag <- if (!is.null(selectors)) selectors$title_tag else NULL
  author_element <- if (!is.null(selectors)) selectors$author_element else NULL
  author_tag <- if (!is.null(selectors)) selectors$author_tag else NULL
  date_element <- if (!is.null(selectors)) selectors$date_element else NULL
  date_tag <- if (!is.null(selectors)) selectors$date_tag else NULL
  text_element <- if (!is.null(selectors)) selectors$text_element else NULL
  text_tag <- if (!is.null(selectors)) selectors$text_tag else NULL

  df <- data.frame(
    url = article_url,
    published_date = sa_extract_date(article_html, date_element, date_tag),
    author = sa_extract_author(article_html, author_element, author_tag),
    title = sa_extract_title(article_html, title_element, title_tag),
    text = sa_extract_text(article_html, text_element, text_tag),
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Scrape Multiple Articles
#'
#' @param article_urls A character vector containing multiple article URLs.
#' @param verbose Logical; if TRUE, prints progress updates and execution time (default: TRUE).
#' @return A data frame where each row represents an article.
#' @import dplyr
#' @import purrr
#' @import tictoc
#' @export
sa_scrape_articles <- function(article_urls, verbose = TRUE) {
  if (length(article_urls) == 0) {
    stop("No URLs provided.")
  }

  total_articles <- length(article_urls)

  if (verbose) {
    message(sprintf("Starting scrape of %d articles...", total_articles))
    tic()
  }

  domains <- unique(gsub("^www\\.", "", stringr::str_extract(article_urls, "(?<=://)([^/]+)")))

  domain_selectors <- setNames(
    map(domains, function(domain) {
      selectors <- sa_get_selectors(paste0("https://", domain))
      return(selectors)
    }),
    domains
  )

  articles_df <- map_dfr(seq_along(article_urls), function(i) {
    url <- article_urls[i]

    if (verbose) {
      message(sprintf("Scraping article %d of %d: %s", i, total_articles, url))
    }

    domain <- gsub("^www\\.", "", stringr::str_extract(url, "(?<=://)([^/]+)"))
    selectors <- domain_selectors[[domain]]

    sa_get_article_data(url, selectors)
  })

  if (verbose) {
    elapsed_time <- toc(quiet = TRUE)
    message(sprintf("Scraped %d articles in %.2f seconds.", total_articles, elapsed_time$toc - elapsed_time$tic))
  }

  return(articles_df)
}


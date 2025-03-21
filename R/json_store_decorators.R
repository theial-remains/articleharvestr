#' Ensure that the news site folder exists
store_ensure_folders <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    folder <- file.path("inst/extdata/article_data", news_site)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    next_fn(data, news_site, overwrite)
  }
}


#' Write metadata to index.json (no article text)
store_index_json <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    folder <- file.path("inst/extdata/article_data", news_site)
    path <- file.path(folder, "index.json")

    minimal <- dplyr::select(data, url, published_date, sentiment_val, sentiment_sd)
    minimal <- dplyr::distinct(minimal, url, .keep_all = TRUE)

    existing <- if (file.exists(path)) jsonlite::read_json(path, simplifyVector = TRUE) else tibble::tibble()

    if (!is.null(existing) && "url" %in% names(existing)) {
      combined <- dplyr::bind_rows(existing, minimal) %>%
        dplyr::group_by(url) %>%
        dplyr::summarise(across(everything(), ~ dplyr::coalesce(last(na.omit(.x)), first(.x))), .groups = "drop")
    } else {
      combined <- minimal
    }

    jsonlite::write_json(combined, path, pretty = TRUE, auto_unbox = TRUE)
    next_fn(data, news_site, overwrite)
  }
}


#' Write full articles to monthly JSONs (YYYY-MM.json)
store_monthly_json <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    folder <- file.path("inst/extdata/article_data", news_site)

    data$month <- format(as.Date(data$published_date), "%Y-%m")
    grouped <- dplyr::group_split(data, data$month)

    for (group in grouped) {
      month_val <- unique(group$month)
      json_path <- file.path(folder, paste0(month_val, ".json"))

      existing <- if (file.exists(json_path)) {
        jsonlite::read_json(json_path, simplifyVector = TRUE)
      } else {
        tibble::tibble()
      }

      if (!is.null(existing) && "url" %in% names(existing)) {
        if (overwrite) {
          existing <- dplyr::anti_join(existing, group, by = "url")
        }

        combined <- dplyr::bind_rows(existing, group) %>%
          dplyr::group_by(url) %>%
          dplyr::summarise(across(everything(), ~ dplyr::coalesce(last(na.omit(.x)), first(.x))), .groups = "drop")
      } else {
        combined <- group
      }

      jsonlite::write_json(combined, json_path, pretty = TRUE, auto_unbox = TRUE)
    }

    next_fn(data, news_site, overwrite)
  }
}

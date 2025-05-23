# AHAHAHAH THESE ARENT USER FACING I CAN WRITE WHATEVER THE FUCK I WANT

# ok so these each
# inputs the next function in the chain
# creates/returns a new function
# with the same params: data, news_site, overwrite
# does something
# calls the next function in the chain (the one that was input)


#' make sure that the news site folder exists or else before writing files
store_ensure_folders <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    # get the folder path for this news site
    folder <- file.path("inst/extdata/article_data", news_site)

    # create the folder if it doesn't already exist
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    # pass control to the next decorator in the chain
    next_fn(data, news_site, overwrite)
  }
}

#' write minimal metadata (no text) to index.json for correct news site
#' add sentiment_val and sentiment_sd columns if they don't exist
store_index_json <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    folder <- file.path("inst/extdata/article_data", news_site)
    path <- file.path(folder, "index.json")

    # ensure sentiment cols exist
    if (!"sentiment_val" %in% names(data)) data$sentiment_val <- NA
    if (!"sentiment_sd" %in% names(data)) data$sentiment_sd <- NA

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

    written_n <- nrow(combined) - if (!is.null(existing)) nrow(existing) else 0
    message("index.json: ", written_n, " new/updated rows")

    next_fn(data, news_site, overwrite)
  }
}

# store_monthly_json writes full article data (with text) to per-month JSON files
# kinda evil to have so many jsons but oh well
store_monthly_json <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    folder <- file.path("inst/extdata/article_data", news_site)
    data$month <- format(as.Date(data$published_date), "%Y-%m")
    grouped <- dplyr::group_split(data, data$month)

    for (group in grouped) {
      month_val <- unique(group$month)
      json_path <- file.path(folder, paste0(month_val, ".json"))
      existing <- if (file.exists(json_path)) jsonlite::read_json(json_path, simplifyVector = TRUE) else tibble::tibble()

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

      new_n <- nrow(combined) - if (!is.null(existing)) nrow(existing) else 0
      message(month_val, ".json: ", new_n, " new/updated rows")
    }

    next_fn(data, news_site, overwrite)
  }
}


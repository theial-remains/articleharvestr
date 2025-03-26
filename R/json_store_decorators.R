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
#' smart merge(?) preferring new values
store_index_json <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    # define the folder and path to index.json for news site
    folder <- file.path("inst/extdata/article_data", news_site)
    path <- file.path(folder, "index.json")

    # select only relevant metadata cols
    minimal <- dplyr::select(data, url, published_date, sentiment_val, sentiment_sd)

    # remove duplicate urls just in case
    keep_cols <- intersect(c("url", "published_date", "sentiment_val", "sentiment_sd"), names(data))
    minimal <- dplyr::select(data, dplyr::all_of(keep_cols))

    # load existing index.json or fallback to empty tibble
    existing <- if (file.exists(path)) jsonlite::read_json(path, simplifyVector = TRUE) else tibble::tibble()

    # merge new and existing metadata if any
    if (!is.null(existing) && "url" %in% names(existing)) {
      combined <- dplyr::bind_rows(existing, minimal) %>%
        dplyr::group_by(url) %>%
        dplyr::summarise(across(everything(), ~ dplyr::coalesce(last(na.omit(.x)), first(.x))), .groups = "drop")
    } else {
      combined <- minimal
    }

    # write the updated/new index.json file
    jsonlite::write_json(combined, path, pretty = TRUE, auto_unbox = TRUE)

    next_fn(data, news_site, overwrite)
  }
}

# store_monthly_json writes full article data (with text) to per-month JSON files
# kinda evil to have so many jsons but oh well
store_monthly_json <- function(next_fn) {
  function(data, news_site, overwrite = FALSE) {
    folder <- file.path("inst/extdata/article_data", news_site)

    # extract month from published_date col
    data$month <- format(as.Date(data$published_date), "%Y-%m")

    # split the data into separate tibbles for each month
    grouped <- dplyr::group_split(data, data$month)

    for (group in grouped) {
      # get the month string (all rows in each group have the same month)
      month_val <- unique(group$month)

      # define json path for that month
      json_path <- file.path(folder, paste0(month_val, ".json"))

      # load existing monthly data/fallback to empty tibble
      existing <- if (file.exists(json_path)) {
        jsonlite::read_json(json_path, simplifyVector = TRUE)
      } else {
        tibble::tibble()
      }

      # if old data exists
      # rm duplicates if overwrite is TRUE
      if (!is.null(existing) && "url" %in% names(existing)) {
        if (overwrite) {
          existing <- dplyr::anti_join(existing, group, by = "url")
        }

        # combine old and new data, preferring most complete row per url
        combined <- dplyr::bind_rows(existing, group) %>%
          dplyr::group_by(url) %>%
          dplyr::summarise(across(everything(), ~ dplyr::coalesce(last(na.omit(.x)), first(.x))), .groups = "drop")
      } else {
        combined <- group
      }

      # write the monthly json file, create or update
      jsonlite::write_json(combined, json_path, pretty = TRUE, auto_unbox = TRUE)
    }

    next_fn(data, news_site, overwrite)
  }
}


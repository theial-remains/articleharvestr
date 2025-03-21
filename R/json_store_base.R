#' Base storage function (no-op)
#'
#' This is the base of the decorator chain. It simply returns NULL and does nothing.
#' @return A storage function that takes (data, news_site, overwrite) and does nothing.
store_base <- function() {
  function(data, news_site, overwrite = FALSE) {
    invisible(NULL)
  }
}

#' Compose a storage pipeline from multiple decorators
#'
#' @param ... Decorator functions (e.g. store_index_json, store_monthly_json)
#' @return A composed function taking (data, news_site, overwrite)
compose_storage <- function(...) {
  decorators <- list(...)
  Reduce(function(f, g) g(f), decorators, right = TRUE)
}

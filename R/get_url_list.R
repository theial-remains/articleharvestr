# returns list of links from article sitemap
# Helper Functions ----------------------------------------------------------

# TODO link list from nytimes





# Main Functions ----------------------------------------------------------

#' scrapes sitemap for list of links in date range
#'
#' @param date_range
#' @return list of links filtered by date range
#' @export
scrape_sitemap <- function(date_range) {
  # code goes here
  # make work for nytimes first
  # use function to find sitemap schema elements
}

#
# nytimes only function to get year links, month links, day links in date_range
# uses create csv function from scrape_new_articles to create a csv for website
# if normal article use pull_schema, warning if schema does not exist

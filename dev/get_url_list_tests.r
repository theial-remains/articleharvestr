rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(httr)
library(purrr)

# 2: get urls
# Test Call for gu_parse_xml_sitemap_date_in_url
test_sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
test_start_date <- "2015-01-01"
test_end_date <- "2015-01-31"

test_results <- gu_parse_xml_sitemap_date_in_url(test_sitemap_url,
                                                 test_start_date,
                                                 test_end_date)
test_results


# Test Call for gu_parse_xml_sitemap_date_in_tag
test_sitemap_url <- "https://www.huffpost.com/sitemaps/archive/sitemap-2015-01-10-v1.xml"
test_start_date <- "2015-01-01"
test_end_date <- "2015-01-31"
tag <- ".//lastmod"

test_results <- gu_parse_xml_sitemap_date_in_tag(test_sitemap_url,
                                                 test_start_date,
                                                 test_end_date,
                                                 tag)
test_results

# Test Call for gu_get_schema_info
schema_info <- gu_get_schema_info("https://www.huffpost.com")
schema_info

# Test call for gu_get_layer_instructions
# get intructions for all layers
instructions <- purrr::map_chr(seq_len(nrow(schema_info)), function(i) {
  gu_get_layer_instructions(schema_info[i, , drop = FALSE])
})
instructions


# overall xml non function ver. test
website_url <- "https://www.huffpost.com"
start_date <- "2015-01-01"
end_date <- "2015-01-05"
schema_info <- gu_get_schema_info(website_url)
starting_sitemap <- gs_pull_schema(website_url)$starting_sitemap[1]

result <- purrr::map(link_list, function(link) {
  tryCatch({
    # Call the helper function
    helper_function(link, start_date, end_date, tag)
  }, error = function(e) {
    message("Error processing link: ", link, " - ", e)
    return(character(0))  # Return empty vector for failed links
  })
}) %>% unlist()

result

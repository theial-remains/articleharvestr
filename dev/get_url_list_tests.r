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

# overall xml function ver. test
result_links <- gu_parse_xml("https://www.huffpost.com", "2015-01-01", "2015-01-04")
result_links


# overall xml non function ver. test
website_url <- "https://www.huffpost.com"
start_date <- "2015-01-01"
end_date <- "2015-01-04"
schema_info <- gu_get_schema_info(website_url)
starting_sitemap <- gs_pull_schema(website_url)$starting_sitemap[1]

# layer 1
link_list1 <- tryCatch({
  # Example of how you might process layer one
  gu_parse_xml_sitemap_date_in_url(starting_sitemap, start_date, end_date)
}, error = function(e) {
  message("Error in Layer 1:", e)
  return(character(0))
})
link_list1

# Layer 2: Process links from Layer 1
if (nrow(schema_info) < 2) stop("No row in schema_info for Layer 2.")
tag_2 <- paste0(".//", schema_info$class[2])

helper_function_2 <- gu_parse_xml_sitemap_date_in_tag
link_list2 <- purrr::map(link_list1, function(link) {
  tryCatch({
    print(paste("Processing link (Layer 2):", link))
    helper_function_2(link, start_date, end_date, tag_2)
  }, error = function(e) {
    message("Error processing link (Layer 2): ", link, " - ", e)
    return(character(0))
  })
}) %>% unlist()
link_list2

# Layer 3: Process links from Layer 2
if (nrow(schema_info) < 3) stop("No row in schema_info for Layer 3.")
tag_3 <- paste0(".//", schema_info$class[3])
helper_function_3 <- gu_parse_xml_sitemap_date_in_tag
link_list3 <- purrr::map(link_list2, function(link) {
  tryCatch({
    print(paste("Processing link (Layer 3):", link))
    helper_function_3(link, start_date, end_date, tag_3)
  }, error = function(e) {
    message("Error processing link (Layer 3): ", link, " - ", e)
    return(character(0))
  })
}) %>% unlist()
print("Layer 3 Links (link_list3):")
print(link_list3)

# Layer 4: Process links from Layer 3
if (nrow(schema_info) < 4) stop("No row in schema_info for Layer 4.")
tag_4 <- paste0(".//", schema_info$class[4])
helper_function_4 <- gu_parse_xml_sitemap_date_in_tag
link_list4 <- purrr::map(link_list3, function(link) {
  tryCatch({
    print(paste("Processing link (Layer 4):", link))
    helper_function_4(link, start_date, end_date, tag_4)
  }, error = function(e) {
    message("Error processing link (Layer 4): ", link, " - ", e)
    return(character(0))
  })
}) %>% unlist()
print("Layer 4 Links (link_list4):")
print(link_list4)
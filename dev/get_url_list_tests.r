rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()

library(xml2)
library(httr)

# 2: get urls
# Test Call for gu_parse_xml_sitemap_date_in_url
test_sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
test_start_date <- "2015-01-01"
test_end_date <- "2015-01-31"

test_results <- gu_parse_xml_sitemap_date_in_url(test_sitemap_url, test_start_date, test_end_date)
print(test_results)


# Test Call for gu_parse_xml_sitemap_date_in_tag
test_sitemap_url <- "https://www.huffpost.com/sitemaps/archive/sitemap-2015-01-10-v1.xml"
test_start_date <- "2015-01-01"
test_end_date <- "2015-01-31"

test_results <- gu_parse_xml_sitemap_date_in_tag(test_sitemap_url, test_start_date, test_end_date)
print(test_results)

# Test Call for gu_get_schema_info
huff_schema_info <- gu_get_schema_info("https://www.huffpost.com")
print(huff_schema_info)

# Test call for gu_get_layer_instructions
# Example Schema Info for a Layer
layer_info_1 <- data.frame(number = 1, type = "url_date", class = NA, stringsAsFactors = FALSE)
layer_info_2 <- data.frame(number = 2, type = "tag_date", class = "lastmod", stringsAsFactors = FALSE)

# Test the helper function
instruction_1 <- gu_get_layer_instructions(layer_info_1)
instruction_2 <- gu_get_layer_instructions(layer_info_2)

# Print instructions
print(instruction_1) # Expected: "single gu_parse_xml_sitemap_date_in_url"
print(instruction_2) # Expected: "map gu_parse_xml_sitemap_date_in_tag"


# Test call for overall xml function


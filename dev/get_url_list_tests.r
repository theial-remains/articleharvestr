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
print(test_results)


# Test Call for gu_parse_xml_sitemap_date_in_tag
test_sitemap_url <- "https://www.huffpost.com/sitemaps/archive/sitemap-2015-01-10-v1.xml"
test_start_date <- "2015-01-01"
test_end_date <- "2015-01-31"
tag <- ".//lastmod"

test_results <- gu_parse_xml_sitemap_date_in_tag(test_sitemap_url,
                                                 test_start_date,
                                                 test_end_date,
                                                 tag)
print(test_results)

# Test Call for gu_get_schema_info
schema_info <- gu_get_schema_info("https://www.huffpost.com")
print(schema_info)

# Test call for gu_get_layer_instructions
# get intructions for all layers
instructions <- purrr::map_chr(seq_len(nrow(schema_info)), function(i) {
  gu_get_layer_instructions(schema_info[i, , drop = FALSE])
})

print(instructions)

# Test call for overall xml function
final_links <- gu_parse_xml_to_article("https://www.huffpost.com")
print(final_links)


# overall xml non function ver. test
# Step 1: Define parameters for testing
website_url <- "https://www.huffpost.com"
start_date <- "2015-01-01"
end_date <- "2015-01-31"

# Step 2: Get schema info
schema_info <- gu_get_schema_info(website_url)

# Include starting_sitemap explicitly
starting_sitemap <- gs_pull_schema(website_url)$starting_sitemap[1]

# Stop if no schema info is found
if (nrow(schema_info) == 0) {
  stop("No schema information found for the given website.")
}

# Print the schema info for debugging
print("Schema Info:")
print(schema_info)

# Initialize the list of links to process
link_list <- NULL

# Step 3-5: Process layers sequentially
for (i in seq_len(nrow(schema_info))) {
  # Extract the row for the current layer
  layer_info <- schema_info[i, , drop = FALSE]

  # Print the current layer info for debugging
  print(paste("Processing Layer", i))
  print(layer_info)

  # Get the helper function for this layer
  helper_function_name <- gu_get_layer_instructions(layer_info)
  helper_function <- match.fun(helper_function_name)

  # Extract the tag (if applicable) from the schema info
  tag <- if (!is.na(layer_info$class)) layer_info$class else NULL

  # Print the helper function name and tag for debugging
  print(paste("Helper Function:", helper_function_name))
  print(paste("Tag:", tag))

  # Apply the function based on the layer
  if (i == 1) {
    # Layer 1: Apply the function to a single sitemap URL
    link_list <- helper_function(starting_sitemap, start_date, end_date)
  } else {
    # Layers 2–4: Apply the function to a list of links using purrr::map
    if (is.null(link_list) || length(link_list) == 0) {
      stop(paste("No links to process for layer", i, "."))
    }
    link_list <- purrr::map(link_list, function(link) {
      helper_function(link, start_date, end_date, tag)
    }) %>% unlist()
  }

  # Print the current list of links for debugging
  print(paste("Links after Layer", i, ":"))
  print(link_list)
}

# Final output: The list of article links
print("Final List of Links:")
print(link_list)

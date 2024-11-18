rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(httr)
library(purrr)

# test calls courtesy of chatgpt lol
# Check if a CSV file exists for the website
su_check_csv("https://www.huffpost.com",
             folder_path = "inst/extdata/scraped_data/")

# Create a CSV file for the website
su_create_csv("https://www.huffpost.com",
              folder_path = "inst/extdata/scraped_data/",
              overwrite = FALSE)

# Remove the CSV file
su_remove_csv("https://www.huffpost.com",
              folder_path = "inst/extdata/scraped_data/")

# Write result_links to the CSV
result_links <- gu_parse_xml("https://www.huffpost.com",
                             "2015-01-01",
                             "2015-01-04")

su_write_urls("https://www.huffpost.com",
              result_links,
              folder_path = "inst/extdata/scraped_data/")

results <- su_read_csv("https://www.huffpost.com")
results

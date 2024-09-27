# Helper Functions ----------------------------------------------------------

#' Check if a schema already exists for a website
#'
#' This function checks if a schema CSV exists locally for the given website URL.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the schema exists, FALSE otherwise.
#' @export
check_schema <- function(website_url) {
  dev_mode_path <- "inst/extdata/website_schemas.csv" # FIXME rm
  schema_file_path <- system.file("extdata", "website_schemas.csv", package = "articleharvestr")

  # Use dev path if it exists; otherwise, use system file path
  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)

  # Create website name key
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))

  # Check if key already exists
  schema_exists <- any(tolower(website_schemas$website) == website_key)
  return(schema_exists)
}

check_schema("aaa")


#' Write a new schema to the local CSV file
#'
#' This function writes a new schema (author, title, published date, article text)
#' to the local CSV file if it does not already exist.
#' @param website_url A character string representing the URL of the website.
#' @param schema_data A list containing the schema elements: author, title, published date, and article text.
#' @return TRUE if the schema is successfully written, FALSE otherwise.
#' @export
write_schema <- function(website_url, schema_data) {
  # Function logic goes here
}

#' Pull schema elements for a website
#'
#' This function retrieves the schema elements for a given website from the local CSV file.
#' @param website_url A character string representing the URL of the website.
#' @return A list of schema elements (author, title, published date, article text) if available, NULL otherwise.
#' @export
pull_schema <- function(website_url) {
  # Function logic goes here
}

#' Prompt the user to overwrite an existing schema
#'
#' This function prompts the user for permission to overwrite an existing schema in the local CSV file.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the user chooses to overwrite, FALSE otherwise.
#' @export
prompt_overwrite <- function(website_url) {
  # Function logic goes here
}

# Main Functions -----------------------------------------------------------

#' Manually find and add schema elements for a website
#'
#' This function allows the user to manually input the schema elements (author, title, published date, article text) for a given website.
#' @param website_url A character string representing the URL of the website.
#' @return A list containing the schema elements (author, title, published date, article text).
#' @export
find_schema_manual <- function(website_url) {
  # Function logic goes here
}

#' Automatically find and add schema elements for a website
#'
#' This function attempts to automatically find schema elements (author, title, published date, article text) for the given website.
#' @param website_url A character string representing the URL of the website.
#' @return A list containing the schema elements (author, title, published date, article text).
#' @export
write_schema_auto <- function(website_url) {
  # Function logic goes here
}

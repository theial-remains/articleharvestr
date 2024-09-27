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

  # Dev path logic
  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))

  # Check if key already exists
  schema_exists <- any(tolower(website_schemas$key) == website_key)
  return(schema_exists)
}


#' Prompt the user to overwrite an existing schema
#'
#' This function prompts the user for permission to overwrite an existing schema in the local CSV file.
#' It displays the existing schema to the user before prompting for confirmation.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the user chooses to overwrite, FALSE otherwise.
#' @export
prompt_overwrite <- function(website_url) {
  # Check if the schema exists
  if (!check_schema(website_url)) {
    return(FALSE)
  }

  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata",
                                  "website_schemas.csv",
                                  package = "articleharvestr")

  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))
  schema_row <- website_schemas[tolower(website_schemas$key) == website_key, ]

  # Display the existing schema to the user
  message("Existing schema for this website:")
  print(schema_row)

  # Prompt the user for confirmation to overwrite
  response <- readline(prompt = "Do you want to overwrite this schema? (y/n): ")

  # Check user response
  if (tolower(response) == "yes") {
    return(TRUE)
  } else {
    message("Schema not overwritten.")
    return(FALSE)
  }
}


#' Prompt the user to overwrite an existing schema
#'
#' This function prompts the user for permission to overwrite an existing schema in the local CSV file.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the user chooses to overwrite, FALSE otherwise.
#' @export
prompt_overwrite <- function(website_url) {
  if (!check_schema(website_url)) {
    message("No existing schema found for this website.")
    return(FALSE)
  }

  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata",
                                  "website_schemas.csv",
                                  package = "articleharvestr")

  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found.")
  }

  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))
  schema_row <- website_schemas[tolower(website_schemas$key) == website_key, ]

  message("Existing schema for this website:")
  print(schema_row)

  response <- readline(prompt = "Do you want to overwrite this schema? (yes/no): ")

  if (tolower(response) %in% c("y", "yes")) {
    return(TRUE)
  } else {
    message("Schema not overwritten.")
    return(FALSE)
  }
}


#' Pull schema elements for a website
#'
#' This function retrieves the schema elements for a given website from the local CSV file.
#' @param website_url A character string representing the URL of the website.
#' @return A list of schema elements (author_element, title_element, date_element, text_element) if available, NULL otherwise.
#' @export
pull_schema <- function(website_url) {
  if (!check_schema(website_url)) {
    message("No schema found for the given website.")
    return(NULL)
  }

  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata", "website_schemas.csv", package = "articleharvestr")

  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))

  # Get schema row
  schema_row <- website_schemas[tolower(website_schemas$key) == website_key, ]

  schema_elements <- list(
    author_element = schema_row$author_element,
    title_element = schema_row$title_element,
    date_element = schema_row$date_element,
    text_element = schema_row$text_element
  )
  return(schema_elements)
}


# Main Functions -----------------------------------------------------------

#' Automatically find and add schema elements for a website
#'
#' This function attempts to automatically find schema elements (author, title, published date, article text) for the given website.
#' @param website_url A character string representing the URL of the website.
#' @return A list containing the schema elements (author, title, published date, article text).
#' @export
write_schema_auto <- function(website_url) {
  # Function logic goes here
}

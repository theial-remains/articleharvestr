# push and pull from website_schemas.csv, find website elements to store
# Helper Functions ----------------------------------------------------------

#' Check if a schema already exists for a website
#'
#' This function checks if a schema CSV exists locally for the given website URL.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the schema exists, FALSE otherwise.
#' @export
gs_check_schema <- function(website_url) {
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


#' title
#'
#' description
#' @param website_url A character string representing the URL of the website.
#' @return what it returns
#' @export
gs_rm_schema <- function(website_url) {
  # TODO use gs_check_schema, the find and remove
}


#' Prompt the user to overwrite an existing schema
#'
#' This function prompts the user for permission to overwrite an existing schema in the local CSV file.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the user chooses to overwrite, FALSE otherwise.
#' @export
gs_prompt_overwrite <- function(website_url) {
  if (!gs_check_schema(website_url)) {
    message("No existing schema found for this website.")
    return(FALSE)
  }

  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata", "website_schemas.csv", package = "articleharvestr")

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

  response <- readline(prompt = "Do you want to overwrite this schema? (y/n): ")

  if (tolower(response) %in% c("y", "yes")) {
    return(TRUE)
  } else {
    message("Schema not overwritten.")
    return(FALSE)
  }
}


#' Write a new schema to the local CSV file
#'
#' This function writes a new schema to the local CSV file. If the schema already exists,
#' it prompts the user for confirmation to overwrite.
#' @param website_url A character string for the URL of the website.
#' @param author_element A character string for the CSS selector/XPath for the author element.
#' @param title_element A character string for the CSS selector/XPath for the title element.
#' @param date_element A character string for the CSS selector/XPath for the published date element.
#' @param text_element A character string for the CSS selector/XPath for the article text element.
#' @param xml_structure A character string for the website structure
#' @return TRUE if the schema is successfully written, FALSE otherwise.
#' @export
gs_write_schema <- function(website_url,
                            author_element,
                            title_element,
                            date_element,
                            text_element,
                            xml_structure) {
  # Check if the schema already exists
  if (gs_check_schema(website_url)) {
    if (!gs_prompt_overwrite(website_url)) {
      return(FALSE)
    }
  }

  # Paths for development and installed package
  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata",
                                  "website_schemas.csv",
                                  package = "articleharvestr")

  # Use the development path if it exists; otherwise, use the system file path
  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  # Read the current schema file
  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)

  # Create website key
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))

  # Remove the old schema row if it exists
  website_schemas <- website_schemas[tolower(website_schemas$key) != website_key, ]

  # Create a new row for the schema
  new_schema_row <- data.frame(
    website_structure = website_url,
    author_element = author_element,
    title_element = title_element,
    date_element = date_element,
    text_element = text_element,
    key = website_key, # Created earlier in function
    xml_structure = xml_structure,
    stringsAsFactors = FALSE
  )

  # Append the new row to the schema data frame
  updated_schemas <- rbind(website_schemas, new_schema_row)

  # Write the updated data frame back to the CSV file
  write.csv(updated_schemas, schema_file_path, row.names = FALSE)
  message("New schema successfully written to the CSV file.")
  return(TRUE)
}


#' Pull schema elements for a website
#'
#' This function retrieves the schema elements for a given website from the local CSV file.
#' @param website_url A character string representing the URL of the website.
#' @return A list of schema elements (author_element, title_element, date_element, text_element) if available, NULL otherwise.
#' @export
gs_pull_schema <- function(website_url) {
  if (!gs_check_schema(website_url)) {
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
    text_element = schema_row$text_element,
    key = schema_row$key,
    xml_structure = schema_row$xml_structure
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
gs_find_article_elements <- function(website_url) {
  # TODO finish
}


#' title
#'
#' description
#' @param sitemap_url parameter desc
#' @return what it returns
#' @export
gs_get_sitemap_str <- function(sitemap_url) {
  # TODO code goes here
}
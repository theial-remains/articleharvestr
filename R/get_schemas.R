# push and pull from website_schemas.csv, find website elements to store


# Helper Functions ----------------------------------------------------------


#' Check if a schema already exists for a website
#'
#' This function checks if a schema CSV exists locally for the given website URL.
#' @param website_url A character string representing the URL of the website.
#' @return TRUE if the schema exists, FALSE otherwise.
#' @importFrom utils read.csv
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


#' Write a New Schema to the Local CSV File
#'
#' This function writes a new schema to the local CSV file.
#' @param website_structure A character string for the URL of the website.
#' @param starting_sitemap A character string for the URL of the starting sitemap.
#' @param author_element A character string for the CSS selector/XPath for the author element.
#' @param title_element A character string for the CSS selector/XPath for the title element.
#' @param date_element A character string for the CSS selector/XPath for the published date element.
#' @param text_element A character string for the CSS selector/XPath for the article text element.
#' @param structure A character string for the sitemap structure (e.g., nested, flat).
#' @param layer1_type, layer1_class, layer2_type, layer2_class, layer3_type, layer3_class, layer4_type, layer4_class Element types and classes for sitemap layers.
#' @return TRUE if the schema is successfully written, FALSE otherwise.
#' @importFrom utils read.csv write.csv
#' @export
gs_write_schema <- function(website_structure,
                            starting_sitemap,
                            author_element,
                            title_element,
                            date_element,
                            text_element,
                            structure,
                            layer1_type = NA,
                            layer1_class = NA,
                            layer2_type = NA,
                            layer2_class = NA,
                            layer3_type = NA,
                            layer3_class = NA,
                            layer4_type = NA,
                            layer4_class = NA) {

  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata", "website_schemas.csv", package = "articleharvestr")

  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)

  new_schema_row <- data.frame(
    website_structure = website_structure,
    starting_sitemap = starting_sitemap,
    author_element = author_element,
    title_element = title_element,
    date_element = date_element,
    text_element = text_element,
    key = tolower(gsub("https://|http://|www\\.|\\..*", "", website_structure)),
    id = paste0(tolower(gsub("https://|http://|www\\.|\\..*", "", website_structure)), "_", as.integer(Sys.time()), "_", sample(1:10000, 1)),
    structure = structure,
    layer1_type = layer1_type,
    layer1_class = layer1_class,
    layer2_type = layer2_type,
    layer2_class = layer2_class,
    layer3_type = layer3_type,
    layer3_class = layer3_class,
    layer4_type = layer4_type,
    layer4_class = layer4_class,
    stringsAsFactors = FALSE
  )

  if (!all(names(new_schema_row) == names(website_schemas))) {
    stop("Column names in new row do not match the existing schema CSV file.")
  }

  updated_schemas <- rbind(website_schemas, new_schema_row)
  write.csv(updated_schemas, schema_file_path, row.names = FALSE)
  message("New schema successfully written to the CSV file.")
  return(TRUE)
}


#' Pull Schema Elements for a Website
#'
#' This function retrieves the schema elements for a given website from the local CSV file.
#' If multiple rows exist for the same website key and no `id` is specified, it returns all rows.
#' If an `id` is provided, it returns only the row corresponding to that `id`.
#' @param website_url A character string representing the URL of the website.
#' @param id A character string specifying the unique ID of the schema to pull (optional).
#' @return A data frame of schema elements. If no rows are found, returns NULL.
#' @importFrom utils read.csv
#' @export
gs_pull_schema <- function(website_url, id = NULL) {
  if (!gs_check_schema(website_url)) {
    stop("No schema found for the given website.")
    return(NULL)
  }

  # Paths for development and installed package
  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata", "website_schemas.csv", package = "articleharvestr")

  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  # Read the current schema file
  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)
  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))

  # Get all schema rows matching the website key
  schema_rows <- website_schemas[tolower(website_schemas$key) == website_key, ]

  # If an ID is specified, filter the rows to that ID
  if (!is.null(id)) {
    schema_rows <- schema_rows[schema_rows$id == id, ]
    if (nrow(schema_rows) == 0) {
      message("No schema found for the specified ID.")
      return(NULL)
    }
  }

  # Return the data frame of schema rows
  return(schema_rows)
}


#' Remove Schema Row(s) from the Local CSV File
#'
#' This function removes schema row(s) from the local CSV file based on the specified `website_url` and optional `id`.
#' If `every = TRUE`, it deletes all rows returned by `gs_pull_schema`.
#' If `every = FALSE` (default), it prints an error if multiple rows are found and returns without deleting.
#' @param website_url A character string representing the URL of the website.
#' @param id An optional character string specifying the unique ID of the schema to remove.
#' @param every Logical; if TRUE, deletes all rows returned by `gs_pull_schema`. Default is FALSE.
#' @return TRUE if the row(s) are successfully removed and the updated CSV file is written, FALSE otherwise.
#' @importFrom utils read.csv write.csv
#' @export
gs_remove_schema <- function(website_url, id = NULL, every = FALSE) {
  # Use gs_pull_schema to get the rows to delete
  schema_rows_to_delete <- gs_pull_schema(website_url, id)

  # If no rows were returned by gs_pull_schema, exit
  if (is.null(schema_rows_to_delete) || nrow(schema_rows_to_delete) == 0) {
    stop()
  }

  # Check if multiple rows were returned and handle based on `every` argument
  if (nrow(schema_rows_to_delete) > 1) {
    if (!every) {
      stop("Specify `id` or set `every = TRUE` to delete all rows.")
    } else {
      message("Deleting multiple rows")
    }
  }

  # Paths for development and installed package
  dev_mode_path <- "inst/extdata/website_schemas.csv"
  schema_file_path <- system.file("extdata", "website_schemas.csv", package = "articleharvestr")

  if (file.exists(dev_mode_path)) {
    schema_file_path <- dev_mode_path
  } else if (schema_file_path == "") {
    stop("Local schema file not found. Ensure 'website_schemas.csv' exists in the 'inst/extdata/' directory.")
  }

  # Read the current schema file
  website_schemas <- read.csv(schema_file_path, stringsAsFactors = FALSE)

  # Remove the rows from the schema file based on the returned rows from gs_pull_schema
  rows_to_keep <- !website_schemas$id %in% schema_rows_to_delete$id
  updated_schemas <- website_schemas[rows_to_keep, ]

  # Write the updated data frame back to the CSV file
  write.csv(updated_schemas, schema_file_path, row.names = FALSE)
  message("Schema row(s) successfully removed from the CSV file:")
  return(TRUE)
}


# Main Functions -----------------------------------------------------------

#' Automatically find and add schema elements for a website
#'
#' This function attempts to automatically find schema elements (author, title, published date, article text) for the given website.
#' @param website_url A character string representing the URL of the website.
#' @export
gs_find_article_elements <- function(website_url) {
  # TODO finish
}


#' title
#'
#' description
#' @param sitemap_url parameter desc
#' @export
gs_get_sitemap_str <- function(sitemap_url) {
  # TODO code goes here
}
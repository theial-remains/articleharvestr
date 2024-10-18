# push and pull link list to csv
# push and pull scraped elements to correct article link
# create and check article key


#' Check if File Exists for a Specific News Website
#'
#' This function checks if a file with a name based on the website key exists
#' in the specified folder. The file should match the key element pulled from
#' the schema by the `gs_pull_schema` function.
#'
#' @param website_url A character string representing the URL of the website.
#' @param id A character string specifying the unique ID of the schema to pull (optional).
#' @param folder_path A character string specifying the folder where the file should be located.
#' @param return_path A logical value indicating whether to return the file path if the file exists. Defaults to FALSE.
#' @return TRUE if the file exists, or FALSE if it does not. If `return_path` is TRUE, returns a list with `exists = TRUE` and the `path` to the file.
#' @importFrom utils file.exists
#' @export
su_check_csv <- function(website_url, id = NULL, folder_path = "inst/extdata/scraped_data/", return_path = FALSE) {
  schema <- gs_pull_schema(website_url, id)

  if (is.null(schema)) {
    stop("No schema found for the given website.")
  }

  website_key <- tolower(gsub("https://|http://|www\\.|\\..*", "", website_url))
  folder_path <- sub("/$", "", folder_path)  # Remove trailing slash if present

  file_name <- paste0(website_key, ".csv")
  file_path <- file.path(folder_path, file_name)

  if (file.exists(file_path)) {
    if (return_path) {
      return(list(exists = TRUE, path = file_path))  # Return a list when return_path is TRUE
    } else {
      return(TRUE)  # Return TRUE if return_path is FALSE
    }
  } else {
    if (return_path) {
      return(list(exists = FALSE, path = file_path))  # Return path even if the file doesn't exist
    } else {
      return(FALSE)
    }
  }
}


#' Add CSV for a Specific News Website
#'
#' This function creates a new CSV file for a website based on the website key.
#' If a file with the same name already exists, the function will either overwrite
#' it (if specified) or skip the creation.
#'
#' @param website_url A character string representing the URL of the website.
#' @param folder_path A character string specifying the folder where the file should be saved.
#' @param overwrite A logical value indicating whether to overwrite an existing file. Defaults to FALSE.
#' @return The full path to the created CSV file, or a message if the file already exists and overwrite is FALSE.
#' @importFrom utils write.csv
#' @export
su_create_csv <- function(website_url, folder_path = "inst/extdata/scraped_data/", overwrite = FALSE) {
  file_info <- su_check_csv(website_url, folder_path = folder_path, return_path = TRUE)

  if (file_info$exists && !overwrite) {
    message("File already exists. Use overwrite = TRUE to replace the existing file.")
    return(file_info$path)
  }

  file_path <- file_info$path
  columns <- c("url", "published_date", "author", "title", "text")
  data <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(data) <- columns
  write.csv(data, file_path, row.names = FALSE)

  return(file_path)
}



#' Remove CSV for a Specific News Website
#'
#' This function removes the CSV file for a website based on the website key.
#' It first checks if the file exists using `su_check_csv`, and if it does,
#' the file is deleted. If the file does not exist, a message is returned.
#'
#' @param website_url A character string representing the URL of the website.
#' @param folder_path A character string specifying the folder where the file is located.
#' @return A message indicating whether the file was successfully removed or if it did not exist.
#' @importFrom utils file.remove
#' @export
su_remove_csv <- function(website_url, folder_path = "inst/extdata/scraped_data/") {
  file_info <- su_check_csv(website_url, folder_path = folder_path, return_path = TRUE)

  if (file_info$exists) {
    file.remove(file_info$path)
    return(paste("File", file_info$path, "successfully removed."))
  } else {
    return("No file found for the given website.")
  }
}


#' Append URLs to CSV for a Specific News Website
#'
#' This function takes a website URL, finds the corresponding CSV file using `su_check_csv`,
#' and appends a list of URLs to new rows in the CSV file. Only the "url" column is filled in.
#' If the file does not exist, it creates a new one.
#'
#' @param website_url A character string representing the URL of the website.
#' @param urls A vector of URLs to be added to the CSV file.
#' @param folder_path A character string specifying the folder where the CSV is located.
#' @return The full path to the CSV file, or a message indicating the result of the operation.
#' @importFrom utils write.csv read.csv
#' @export
su_write_urls <- function(website_url, urls, folder_path = "inst/extdata/scraped_data/") {
  file_info <- su_check_csv(website_url, folder_path = folder_path, return_path = TRUE)
  file_path <- file_info$path
  new_data <- data.frame(url = urls,
                         published_date = NA,
                         author = NA,
                         title = NA,
                         text = NA,
                         stringsAsFactors = FALSE)

  if (file_info$exists) {
    existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
    combined_data <- rbind(existing_data, new_data)

    write.csv(combined_data, file_path, row.names = FALSE)
    message("New URLs appended to the CSV file.")

  } else {
    write.csv(new_data, file_path, row.names = FALSE)
    message("CSV file created and URLs added.")
  }
  return(file_path)
}


#' title
#'
#' description
#' @param param1 parameter desc
#' @return what it returns
#' @export
su_check_urls <- function(param1) {
  # code goes here
}


#' title
#'
#' description
#' @param param1 parameter desc
#' @return what it returns
#' @export
su_create_url_key <- function(param1) {
  # code goes here
}


#' title
#'
#' description
#' @param param1 parameter desc
#' @return what it returns
#' @export
su_push_urls <- function(param1) {
  # code goes here
}


#' title
#'
#' description
#' @param param1 parameter desc
#' @return what it returns
#' @export
su_pull_urls <- function(param1) {
  # code goes here
}
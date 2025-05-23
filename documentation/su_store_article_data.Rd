% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/store_scraped_data.R
\name{su_store_article_data}
\alias{su_store_article_data}
\title{Store Article Data in CSV}
\usage{
su_store_article_data(
  website_url,
  article_data,
  folder_path = "inst/extdata/scraped_data/",
  overwrite = FALSE
)
}
\arguments{
\item{website_url}{A character string representing the website URL.}

\item{article_data}{A data frame containing columns: title, url, author, published_date, and text.}

\item{folder_path}{A character string specifying the folder where the CSV is located. Defaults to \code{"inst/extdata/scraped_data/"}.}

\item{overwrite}{A logical value indicating whether to overwrite existing data for URLs in the CSV. Defaults to FALSE.}
}
\value{
A message indicating success, and the updated CSV is saved to disk.
}
\description{
This function reads the corresponding CSV for the articles using \code{su_check_csv} to find the appropriate file
and appends new article data by matching the URLs from the input data frame to the URLs in the CSV.
If URLs already exist in the CSV, their data can be updated based on the \code{overwrite} parameter.
}

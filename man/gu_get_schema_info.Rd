% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_url_list.R
\name{gu_get_schema_info}
\alias{gu_get_schema_info}
\title{Get Schema Information for a Website}
\usage{
gu_get_schema_info(website_url)
}
\arguments{
\item{website_url}{A character string representing the URL of the website.}
}
\value{
A data frame with columns "number", "type", and "class", where each row represents a layer.
}
\description{
This helper function retrieves and processes schema information for a given website
to prepare for sitemap scraping.
}

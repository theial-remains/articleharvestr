% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_url_list.R
\name{gu_filter_links_by_date}
\alias{gu_filter_links_by_date}
\title{Filter Links by Date}
\usage{
gu_filter_links_by_date(links, level, start_date, end_date)
}
\arguments{
\item{links}{A character vector of URLs to be filtered.}

\item{level}{A character string specifying the date level ("year", "month", or "day").}

\item{start_date}{The start date for filtering (as \code{Date}).}

\item{end_date}{The end date for filtering (as \code{Date}).}
}
\value{
A character vector of URLs that match the specified date level and date range.
}
\description{
This helper function filters a vector of URLs based on the specified date level (e.g., "year", "month", "day").
It extracts dates from URLs in formats such as \code{YYYY}, \code{YYYY/MM}, or \code{YYYY/MM/DD}, allowing for
both forward slashes and hyphens. For day-level links, it further filters URLs by a specified date range.
}

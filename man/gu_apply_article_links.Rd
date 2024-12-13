% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_url_list.R
\name{gu_apply_article_links}
\alias{gu_apply_article_links}
\title{Get All Article Links from a List of Day Links}
\usage{
gu_apply_article_links(day_links, tag_type = NULL, tag_class = NULL)
}
\arguments{
\item{day_links}{A character vector containing the list of day URLs.}

\item{tag_type}{An optional character string specifying the type of HTML tag (e.g., "ol", "ul", "div"). Defaults to "ol".}

\item{tag_class}{An optional character string specifying the class of the tag containing links.}
}
\value{
A list of URLs of all the article links.
}
\description{
This function calls \code{gu_article_links} for each link in a list of day URLs and returns all article links.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_articles.r
\name{sa_extract_text}
\alias{sa_extract_text}
\title{Extract Article Text}
\usage{
sa_extract_text(article_html)
}
\arguments{
\item{article_html}{An rvest HTML document object containing the full article content.}
}
\value{
A character string representing the cleaned article text.
}
\description{
This function extracts the article text, filtering out unwanted content.
}

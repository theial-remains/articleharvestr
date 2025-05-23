% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_sentiment.R
\name{as_sentiment_grouped}
\alias{as_sentiment_grouped}
\title{Perform Sentiment Analysis Grouped by Author, Date, or Both}
\usage{
as_sentiment_grouped(dataframe, group_by = "both")
}
\arguments{
\item{dataframe}{A data frame containing article text, author, and published_date.}

\item{group_by}{Character: "author", "date", or "both" (default: "both").}
}
\value{
A data frame with sentiment scores grouped as specified.
}
\description{
Computes sentiment for articles if not already done, then calculates
sentiment averages grouped by author, date, or both.
}

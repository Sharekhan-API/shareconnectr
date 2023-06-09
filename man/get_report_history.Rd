% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restful.R
\name{get_report_history}
\alias{get_report_history}
\title{Function to get the order history.}
\usage{
get_report_history(object, param)
}
\arguments{
\item{object}{An object of type sharekhanconnect with valid api_key and
access_token.}

\item{param}{This contains the request parameters.}
}
\value{
Returns a json object with order details, if successful.
}
\description{
Retrieve an order's history.
}

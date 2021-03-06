% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request.R
\name{request}
\alias{request}
\alias{get_request}
\title{Generate a request to SetListFM API.}
\usage{
get_request(endpoint, key, params)
}
\arguments{
\item{endpoint}{Endpoint to be reached}

\item{key}{API key.}

\item{params}{Parameters to be added to request.}
}
\value{
\code{request} returns a list with the request
}
\description{
Returns the content of the request
}
\examples{

\dontrun{
key <- "your token"
artist <- "the-beatles"
get_request(endpoint = "search/artists",
key = key,
params = list(artistName = artist))
}

}
\author{
Douglas Zickuhr, \email{douglasrsl@gmail.com}.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_setlists.R
\name{search_setlists}
\alias{search_setlists}
\title{Search Setlists on SetListFM API.}
\usage{
search_setlists(search_params, key = return_key())
}
\arguments{
\item{search_params}{List of parameters to be used on the search}

\item{key}{API key.}
}
\value{
\code{search_artists} returns a tibble with artists data
}
\description{
Returns setlists from SetlistFM API.
}
\examples{

\dontrun{
key <- "your token"
search_term <- "the-beatles"
search_artists(key = key,
search_term = artist)
}

}
\author{
Douglas Zickuhr, \email{douglasrsl@gmail.com}.
}

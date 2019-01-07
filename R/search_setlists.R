#' @name search_setlists
#' @author Douglas Zickuhr, \email{douglasrsl@gmail.com}.
#' @export
#' @title Search Setlists on SetListFM API.
#' @description Returns setlists from SetlistFM API.
#' @param key API key.
#' @param search_params List of parameters to be used on the search
#' @return \code{search_artists} returns a tibble with artists data
#' @examples
#'
#' \dontrun{
#' key <- "your token"
#' search_term <- "the-beatles"
#' search_artists(key = key,
#' search_term = artist)
#' }
#'
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate

search_setlists <- function(search_params, key = return_key()){
  if (missing(search_params)){
    stop("The search params must be specified", call. = FALSE)
  }

  params <- search_params

  setlists <- get_request(endpoint = "search/setlists",
                          key = key,
                          params = params) %>%
    unnest(sets.set) %>%
    clean_names()

  if ("song" %in% colnames(setlists)){
    setlists <- setlists %>%
      mutate(songs = as.list(song)) %>%
      unnest(songs)
  }

  return(setlists)
}

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
#'

search_setlists <- function(key, search_params){
  if (missing(key)) {
    stop("API key must be specified.", call. = FALSE)
  }

  if (missing(search_params)){
    stop("The search params must be specified", call. = FALSE)
  }

  params <- search_params

  setlists <- setlistfmR::get_request(endpoint = "search/setlists",
                                     key = key,
                                     params = params) %>%
    tidyr::unnest(sets.set)

  if ("song"  %in% colnames(setlists)){
    setlists %<>%
      mutate(songs = as.list(song)) %>%
      tidyr::unnest(songs)
  }


  return(setlists)
}

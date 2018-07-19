#' @name search_artists
#' @author Douglas Zickuhr, \email{douglasrsl@gmail.com}.
#' @export
#' @title Search Artists on SetListFM API.
#' @description Returns artists from SetlistFM API.
#' @param key API key.
#' @param search_term Artist to be searched
#' @param search_by Parameter to be searched - Name, Musicbrainz Identifier, Ticketmaster Identifier
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

search_artist <- function(key, search_term, search_by = "artistName"){
  if (missing(key)) {
    stop("API key must be specified.", call. = FALSE)
  }

  if (missing(search_term)){
    stop("The search term must be specified", call. = FALSE)
  }

  if (!search_by %in% c("artistMbid","artistName","artistTmid")){
    stop("Parameter search_by for search/artists must be artistMbid, artistName or artistTmid.", call. = FALSE)
  }


  params <- list()
  params[["sort"]] <- "relevance"
  params[[search_by]] <- search_term

  artists <- setlistfmR::get_request(endpoint = "search/artists",
                                 key = key,
                                 params = params) %>%
    dplyr::distinct()

  return(artists)
}

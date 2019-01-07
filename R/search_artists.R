#' @name search_artist
#'
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
#' @importFrom dplyr distinct
#' @importFrom janitor clean_names

search_artist <- function(search_term, search_by = "artistName", key = return_key()){
  if (missing(search_term)){
    stop("The search term must be specified", call. = FALSE)
  }

  if (!search_by %in% c("artistMbid","artistName","artistTmid")){
    stop("Parameter search_by for search_artist must be artistMbid, artistName or artistTmid.", call. = FALSE)
  }


  params <- list()
  params[["sort"]] <- "relevance"
  params[[search_by]] <- search_term

  artists <- get_request(endpoint = "search/artists",
                         key = key,
                         params = params) %>%
    distinct() %>%
    clean_names()

  return(artists)
}

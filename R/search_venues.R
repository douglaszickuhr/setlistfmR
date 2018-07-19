#' @name search_venues
#' @author Douglas Zickuhr, \email{douglasrsl@gmail.com}.
#' @export
#' @title Search Venues on SetListFM API.
#' @description Returns venues from SetlistFM API.
#' @param key API key.
#' @param search_term Term to be searched
#' @param search_by Parameter to be searched
#' @return \code{search_venues} returns a tibble with venues
#' @examples
#'
#' \dontrun{
#' key <- "your token"
#' artist <- "the-beatles"
#' get_request(key = key,
#' artist = artist)
#' }
#'
#'
#'

search_by_options <- c("cityId","cityName","country","name","state","stateCode")

search_venue <- function(key, search_term, search_by = "cityName"){
  if (missing(key)) {
    stop("API key must be specified.", call. = FALSE)
  }

  if (missing(search_term)){
    stop("The search term must be specified", call. = FALSE)
  }

  if (!search_by %in% search_by_options){
    stop(paste("Parameter search_by for search/venues must be one of the following:",stringr::str_c(search_by_options,collapse = ",")), call. = FALSE)
  }

  params <- list()
  params[[search_by]] <- search_term

  venues <- setlistfmR::get_request(endpoint = "search/venues",
                                     key = key,
                                     params = params) %>%
    dplyr::distinct()

  return(venues)
}

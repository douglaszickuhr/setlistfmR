#' @name search_venues
#' @author Douglas Zickuhr, \email{douglasrsl@gmail.com}.
#' @export
#' @title Search Venues on SetListFM API.
#' @description Returns venues from SetlistFM API.
#' @param key API key.
#' @param search_term Term to be searched
#' @param search_by Parameter to be searched - Options are: cityId, cityName, country, name, state stateCode
#' @return \code{search_venues} returns a tibble with venues according to the query parameters
#' @examples
#'
#' \dontrun{
#' key <- "your token"
#' artist <- "the-beatles"
#' get_request(key = key,
#' artist = artist)
#' }
#'
#' @importFrom stringr str_c
#' @importFrom janitor clean_names
#'
search_venue <- function(search_term, search_by = "cityName", key = return_key()){
  if (missing(search_term)){
    stop("The search term must be specified", call. = FALSE)
  }

  search_by_options <- c("cityId","cityName","country","name","state","stateCode")

  if (!search_by %in% search_by_options){
    stop(paste("Parameter search_by for search/venues must be one of the following:",str_c(search_by_options,collapse = ",")), call. = FALSE)
  }

  params <- list()
  params[[search_by]] <- search_term

  venues <- get_request(endpoint = "search/venues",
                        key = key,
                        params = params) %>%
    distinct() %>%
    clean_names()

  return(venues)
}

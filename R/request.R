#' @name request
#' @author Douglas Zickuhr, \email{douglasrsl@gmail.com}.
#' @export
#' @title Generate a request to SetListFM API.
#' @description Returns the content of the request
#' @param endpoint Endpoint to be reached
#' @param key API key.
#' @param params Parameters to be added to request.
#' @return \code{request} returns a list with the request
#' @examples
#'
#' \dontrun{
#' key <- "your token"
#' artist <- "the-beatles"
#' get_request(endpoint = "search/artists",
#' key = key,
#' params = list(artistName = artist))
#' }
#'
#'

get_request <- function(endpoint, key, params){
  ua <- httr::user_agent("http://github.com/douglaszickuhr")

  if (missing(key)) {
    stop("API key must be specified", call. = FALSE)
  }

  if (missing(endpoint)){
    stop("Endpoint must be specified", call. = FALSE)
  }

  if (missing(params)){
    stop("Query parameters must be specified", call. = FALSE)
  }

  items_per_page <- 0
  total <- 1
  page <- 0
  result <- list()

  while (items_per_page < total){
    page <- page + 1
    params[["p"]] <- page

    req <- httr::GET(paste("https://api.setlist.fm/rest/1.0",endpoint,sep = "/"),
                     httr::add_headers("x-api-key" = key),
                     httr::accept_json(),
                     query = params,
                     ua = ua)

    if (httr::http_error(req)){
      stop(paste("Setlist.FM API failed.",httr::http_status(req)$message), call. = FALSE)
    }

    con <- httr::content(req, encoding = "UTF-8",
                         as = "text",
                         type = "application/json")
    con <- jsonlite::fromJSON(con,flatten = TRUE,simplifyDataFrame = TRUE)

    items_per_page <- items_per_page + con$itemsPerPage
    total <- con$total


    result[[page]] <- con[[length(con)]]
  }

  result %<>%
    purrr::map_df(magrittr::extract) %>%
    dplyr::mutate_if(is.character, funs(replace(., . %in% c("NA","","NULL"), NA))) %>%
    tibble::as.tibble()

  return(result)
}
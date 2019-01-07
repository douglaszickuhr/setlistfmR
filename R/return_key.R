#' Return Setlist FM API Key from environment
#'
#' @return \code{return_key} returns a key stored on the environment
#'
return_key <- function() {
  return(Sys.getenv("setlistfm_key"))
}

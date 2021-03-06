


#' Random String Generator
#'
#' Creates a random string with optional prefix. Useful for unique identifier
#' strings
#'
#' @param prefix optional string to append to the beginning of random string
#'
#' @return single string value
#' @export
#'
#' @examples
#' random_string("rando")
random_string <- function(prefix = "") {
  
  checkmate::assert_string(prefix)
  
  chars <- c(letters, LETTERS, 0:9)
  nchars <- round(stats::runif(1, 8, 12))
  
  paste(c(prefix, sample(chars, nchars, replace = TRUE)), collapse = "")
}

#' 
#' JSON element class.
#' 
#' @export
setClass(
  "json_element",
  representation(
    data="ANY"
  )
)

#' 
#' Instantiate a JSON element.
#' 
#' @param x JSON object representation in R
#' @return JSON element
#' @export
JSONElement <- function(x) {
  return(new("json_element", data=x))
}

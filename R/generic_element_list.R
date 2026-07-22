#_______________________________________________________________________________
#----                         pmx_element class                             ----
#_______________________________________________________________________________

#' 
#' PMX element class.
#' 
#' @export
setClass(
  "pmx_element",
  representation(
  )
)

#_______________________________________________________________________________
#----                           get_name                                    ----
#_______________________________________________________________________________

#' Get element name.
#' 
#' @param x element to know the name
#' @return the name of this element
#' @export
#' @rdname get_name
get_name <- function(x) {
  stop("No default function is provided")
}

setGeneric("get_name", function(x) {
  standardGeneric("get_name")
})
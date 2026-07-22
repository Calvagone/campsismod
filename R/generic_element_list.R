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

setMethod("get_name", signature(x = "ANY"), function(x) {
  # Check if a explicit, non-ANY method exists for 'getName'
  if (isGeneric("getName")) {
    m <- selectMethod("getName", signature(class(x)), optional = TRUE)
    # Ensure a method exists AND it's not the generic default (ANY) method
    if (!is.null(m) && m@defined[["x"]] != "ANY") {
      return(getName(x))
    }
  }
  stop(sprintf("No 'get_name' method defined for class '%s'", class(x)))
})

setGeneric("getName", function(x) {
  standardGeneric("getName")
})

setMethod("getName", signature(x = "ANY"), function(x) {
  .Deprecated("get_name", msg = "'getName' is deprecated. Use 'get_name' instead.")
  
  # Check if a explicit, non-ANY method exists for 'get_name'
  if (isGeneric("get_name")) {
    m <- selectMethod("get_name", signature(class(x)), optional = TRUE)
    if (!is.null(m) && m@defined[["x"]] != "ANY") {
      return(get_name(x))
    }
  }
  stop(sprintf("No 'getName' method defined for class '%s'", class(x)))
})

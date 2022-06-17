#_______________________________________________________________________________
#----                         compartment class                             ----
#_______________________________________________________________________________

#' 
#' Compartment class.
#' 
#' @slot name compartment name (without prefix)
#' @slot index compartment index
#' @export
setClass(
  "compartment",
  representation(
    name = "character",
    index = "integer"
  ),
  contains = "pmx_element",
  prototype = prototype(name=as.character(NA)),
  validity = function(object) {
    return(expectOneForAll(object, c("name", "index")))
  }
)

#' 
#' Create a compartment.
#' 
#' @param index compartment index
#' @param name compartment name (without prefix)
#' @return an empty list of compartments  
#' @export
Compartment <- function(index, name=NA) {
  return(new("compartment", index=as.integer(index), name=as.character(name)))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature=c("compartment"), definition=function(x) {
  return(paste0("A", "_", x@index))
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment"), definition=function(object) {
  cat(paste0(object %>% toString(), " (CMT=", object@index, ")"))
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("compartment"), definition=function(object, ...) {
  if (is.na(object@name)) {
    return(paste0("A", "_", object@index))
  } else {
    return(paste0("A", "_", object@name))
  }
})


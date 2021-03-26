#_______________________________________________________________________________
#----                         compartment class                             ----
#_______________________________________________________________________________

validateCompartment <- function(object) {
  return(expectOneForAll(object, c("name", "index")))
}

#' @export
setClass(
  "compartment",
  representation(
    name = "character",
    index = "integer"
  ),
  contains = "pmx_element",
  prototype = prototype(name=as.character(NA)),
  validity = validateCompartment
)

#' 
#' Create a compartment.
#' 
#' @return an empty list of compartments  
#' @export
Compartment <- function(index, name=NA) {
  return(new("compartment", index=as.integer(index), name=as.character(name)))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("compartment"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("A", "_", x@index))
  } else {
    return(x@name)
  }
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment"), definition=function(object) {
  cat(paste0(object %>% getName(), " (CMT=", object@index, ")"))
})


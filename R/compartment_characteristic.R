#_______________________________________________________________________________
#----                   compartment_characteristic class                    ----
#_______________________________________________________________________________

validateCompartmentCharacteristic <- function(object) {
  return(expectOneForAll(object, c("compartment", "rhs")))
}

#' 
#' Compartment characteristic class.
#' 
#' @export
setClass(
  "compartment_characteristic",
  representation(
    compartment = "integer",
    rhs = "character"
  ),
  contains="pmx_element",
  validity=validateCompartmentCharacteristic 
)

#_______________________________________________________________________________
#----                            getPrefix                                  ----
#_______________________________________________________________________________

#' Get prefix.
#' 
#' @param object generic object
#' @param ... e.g. dest='mrgsolve'
#' @export
getPrefix <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getPrefix", function(object, ...) {
  standardGeneric("getPrefix")
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_characteristic"), definition=function(object) {
  cat(object %>% getName())
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

setMethod("toString", signature=c("compartment_characteristic"), definition=function(object, ...) {
  model <- processExtraArg(args=list(...), name="model", mandatory=TRUE)
  compartmentIndex <- object@compartment
  compartment <- model@compartments %>% getByIndex(Compartment(index=compartmentIndex))
  return(paste0(object %>% getPrefix(), "(", compartment %>% getName(), ")=", object@rhs))
})

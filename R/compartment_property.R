#_______________________________________________________________________________
#----                   compartment_property class                    ----
#_______________________________________________________________________________

validateCompartmentCharacteristic <- function(object) {
  return(expectOneForAll(object, c("compartment", "rhs")))
}

#' 
#' Compartment property class.
#' 
#' @slot compartment related compartment index
#' @slot rhs right-hand side formula
#' @export
setClass(
  "compartment_property",
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
#----                          getRecordName                                ----
#_______________________________________________________________________________

#' Get record name.
#' 
#' @param object generic object
#' @export
getRecordName <- function(object) {
  stop("No default function is provided")
}

setGeneric("getRecordName", function(object) {
  standardGeneric("getRecordName")
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_property"), definition=function(object) {
  cat(paste0(object %>% getName(), ": ", object@rhs))
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

setMethod("toString", signature=c("compartment_property"), definition=function(object, ...) {
  model <- processExtraArg(args=list(...), name="model", mandatory=TRUE)
  dest <- processExtraArg(args=list(...), name="dest", mandatory=TRUE)
  
  compartmentIndex <- object@compartment
  compartment <- model@compartments %>% getByIndex(Compartment(index=compartmentIndex))
  
  if (dest=="RxODE") {
    return(paste0(object %>% getPrefix(dest=dest), "(", compartment %>% getName(), ")=", object@rhs))
  } else if (dest=="mrgsolve") {
    return(paste0(object %>% getPrefix(dest=dest), "_", compartment %>% getName(), "=", object@rhs))
  } else if (dest=="pmxmod") {
    return(paste0(compartment %>% getName(), "=", object@rhs))
  } else {
    stop("Only RxODE, mrgsolve or pmxmod are supported")
  }
})


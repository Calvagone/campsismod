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
    compartment = "compartment",
    rhs = "character"
  ),
  contains="pmx_element",
  validity=validateCompartmentCharacteristic 
)

#_______________________________________________________________________________
#----                              getPrefix                                ----
#_______________________________________________________________________________

#' Get prefix.
#' 
#' @param object generic object
#' @export
getPrefix <- function(object) {
  stop("No default function is provided")
}

setGeneric("getPrefix", function(object) {
  standardGeneric("getPrefix")
})

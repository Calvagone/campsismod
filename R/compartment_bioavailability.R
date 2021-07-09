
#_______________________________________________________________________________
#----                   compartment_bioavailability class                   ----
#_______________________________________________________________________________

validateBioavailability <- function(object) {
  return(TRUE)
}

#' 
#' Compartment bioavailability class.
#' 
#' @export
setClass(
  "compartment_bioavailability",
  representation(
  ),
  contains = "compartment_property",
  validity=validateBioavailability
)

#'
#' Create a bioavailability for the specified compartment.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return bioavailability
#' @export
Bioavailability <- function(compartment, rhs) {
  return(new("compartment_bioavailability", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________


setMethod("getName", signature = c("compartment_bioavailability"), definition = function(x) {
  return(paste0("BIOAVAILABILITY (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

setMethod("getPrefix", signature = c("compartment_bioavailability"), definition = function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="RxODE")
  if (dest=="mrgsolve") {
    return("F")
  } else {
    return("f")
  }
})

#_______________________________________________________________________________
#----                           getRecordName                               ----
#_______________________________________________________________________________

setMethod("getRecordName", signature = c("compartment_bioavailability"), definition = function(object) {
  return("F")
})

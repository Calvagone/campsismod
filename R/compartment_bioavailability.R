
#_______________________________________________________________________________
#----                   compartment_bioavailability class                   ----
#_______________________________________________________________________________

#' 
#' Compartment bioavailability class.
#' 
#' @export
setClass(
  "compartment_bioavailability",
  representation(
  ),
  contains = "compartment_property",
  validity=function(object) {
    return(TRUE)
  }
)

#'
#' Create a bioavailability for the specified compartment.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return a bioavailability property
#' @export
Bioavailability <- function(compartment, rhs="") {
  return(new("compartment_bioavailability", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("compartment_bioavailability"), definition = function(x) {
  return(paste0("BIOAVAILABILITY (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

#' @rdname getPrefix
setMethod("getPrefix", signature = c("compartment_bioavailability"), definition = function(object, ...) {
  dest <- process_extra_arg(args=list(...), name="dest", default="rxode2")
  if (dest=="mrgsolve") {
    return("F")
  } else {
    return("f")
  }
})

#_______________________________________________________________________________
#----                           getRecordName                               ----
#_______________________________________________________________________________

#' @rdname getRecordName
setMethod("getRecordName", signature = c("compartment_bioavailability"), definition = function(object) {
  return("F")
})

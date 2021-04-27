#_______________________________________________________________________________
#----               compartment_initial_condition class                     ----
#_______________________________________________________________________________

validateInitialCondition <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "compartment_initial_condition",
  representation(
  ),
  contains = "compartment_property",
  validity=validateInitialCondition
)

#'
#' Create an initial condition.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return initial condition object
#' @export
InitialCondition <- function(compartment, rhs) {
  return(new("compartment_initial_condition", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature=c("compartment_initial_condition"), definition=function(x) {
  return(paste0("INIT (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                           getRecordName                               ----
#_______________________________________________________________________________

setMethod("getRecordName", signature = c("compartment_initial_condition"), definition = function(object) {
  return("INIT")
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_initial_condition"), definition=function(object) {
  cat(paste0(object %>% getName(), ": ", object@rhs))
})


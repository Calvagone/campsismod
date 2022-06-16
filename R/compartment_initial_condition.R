#_______________________________________________________________________________
#----               compartment_initial_condition class                     ----
#_______________________________________________________________________________

#' 
#' Compartment initial condition class.
#' 
#' @export
setClass(
  "compartment_initial_condition",
  representation(
  ),
  contains = "compartment_property",
  validity=function(object) {
    return(TRUE)
  }
)

#'
#' Create an initial condition.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return an initial condition property
#' @export
InitialCondition <- function(compartment, rhs="") {
  return(new("compartment_initial_condition", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature=c("compartment_initial_condition"), definition=function(x) {
  return(paste0("INIT (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

#' @rdname getPrefix
setMethod("getPrefix", signature = c("compartment_initial_condition"), definition = function(object, ...) {
  return("")
})

#_______________________________________________________________________________
#----                           getRecordName                               ----
#_______________________________________________________________________________

#' @rdname getRecordName
setMethod("getRecordName", signature = c("compartment_initial_condition"), definition = function(object) {
  return("INIT")
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_initial_condition"), definition=function(object) {
  cat(paste0(object %>% getName(), ": ", object@rhs))
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("compartment_initial_condition"), definition=function(object, ...) {
  model <- processExtraArg(args=list(...), name="model", mandatory=TRUE)
  dest <- processExtraArg(args=list(...), name="dest", mandatory=TRUE)
  
  compartmentIndex <- object@compartment
  compartment <- model@compartments %>% find(Compartment(index=compartmentIndex))
  
  if (dest=="RxODE") {
    return(paste0(compartment %>% toString(), "(0)=", object@rhs))
  } else if (dest=="mrgsolve") {
    return(paste0(compartment %>% toString(), "_0=", object@rhs))
  } else if (dest=="pmxmod") {
    return(paste0(compartment %>% toString(), "=", object@rhs))
  } else {
    stop("Only RxODE, mrgsolve or pmxmod are supported")
  }
})


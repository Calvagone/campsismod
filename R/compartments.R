#_______________________________________________________________________________
#----                        compartments class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartments",
  representation(
    characteristics="compartment_characteristics",
    initial_conditions="compartment_initial_conditions"
  ),
  contains = "pmx_list",
  prototype = prototype(type="compartment", characteristics=new("compartment_characteristics"),
                        initial_conditions=new("compartment_initial_conditions"))
)

#' 
#' Create a list of compartments
#' 
#' @return an empty list of compartments  
#' @export
Compartments <- function() {
  return(new("compartments"))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature = c("compartments", "compartment_characteristic"), definition = function(object, x) {
  object@characteristics <- object@characteristics %>% add(x) 
  return(object)
})

setMethod("add", signature = c("compartments", "compartment_initial_condition"), definition = function(object, x) {
  object@initial_conditions <- object@initial_conditions %>% add(x) 
  return(object)
})

#_______________________________________________________________________________
#----                             getByIndex                                ----
#_______________________________________________________________________________

setMethod("getByIndex", signature=c("compartments", "compartment"), definition=function(object, x) {
  retValue <- object@list %>% purrr::keep(~(.x@index==x@index))
  
  if (length(retValue) > 0) {
    retValue <- retValue[[1]]
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                          getCompartmentIndex                          ----
#_______________________________________________________________________________

setMethod("getCompartmentIndex", signature=c("compartments", "character"), definition=function(object, name) {
  compartment <- object %>% getByName(paste0("A_", name))
  if (compartment %>% length() == 0) {
    stop(paste0("Compartment ", name, " not found."))
  }
  return(compartment@index)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartments"), definition=function(object) {
  for (element in object@list) {
    show(element)
    cat("\n")
  }
  if (object@characteristics %>% length() == 0) {
    cat("No compartment characteristic")
  } else {
    cat("Compartment characteristics:\n")
    show(object@characteristics)
  }
  if (object@initial_conditions %>% length() == 0) {
    cat("No initial conditions")
  } else {
    cat("Initial conditions:\n")
    show(object@initial_conditions)
  }
})


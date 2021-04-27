#_______________________________________________________________________________
#----                        compartments class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartments",
  representation(
    properties="compartment_properties"
  ),
  contains = "pmx_list",
  prototype = prototype(type="compartment", properties=new("compartment_properties"))
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

setMethod("add", signature = c("compartments", "compartment_property"), definition = function(object, x) {
  object@properties <- object@properties %>% add(x) 
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
  cat("Compartments:\n")
  for (element in object@list) {
    show(element)
    cat("\n")
  }
})


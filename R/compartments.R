#_______________________________________________________________________________
#----                        compartments class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartments",
  representation(
    characteristics="compartment_characteristics"
  ),
  contains = "pmx_list",
  prototype = prototype(type="compartment", characteristics=new("compartment_characteristics"))
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
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartments"), definition=function(object) {
  for (element in object@list) {
    show(element)
    print("")
  }
  show(object@characteristics)
})


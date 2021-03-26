#_______________________________________________________________________________
#----                        compartments class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartments",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="compartment")
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
#----                             getByIndex                                ----
#_______________________________________________________________________________

setMethod("getByIndex", signature=c("compartments", "compartment"), definition=function(object, x) {
  retValue <- object@list %>% purrr::keep(~(.x@index==x@index))
  
  if (length(retValue) > 0) {
    retValue <- retValue[[1]]
  }
  return(retValue)
})

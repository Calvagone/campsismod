
#_______________________________________________________________________________
#----                       initial_conditions class                        ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartment_initial_conditions",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="compartment_initial_condition") 
)

#_______________________________________________________________________________
#----                             getByIndex                                ----
#_______________________________________________________________________________

setMethod("getByIndex", signature=c("compartment_initial_conditions", "compartment_initial_condition"), definition=function(object, x) {
  retValue <- object@list %>% purrr::keep(~(.x@compartment==x@compartment & as.character(class(.x))==as.character(class(x))))
  
  if (length(retValue) > 0) {
    retValue <- retValue[[1]]
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_initial_conditions"), definition=function(object) {
  for (element in object@list) {
    show(element)
    cat("\n")
  }
})

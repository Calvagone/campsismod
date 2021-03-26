
#_______________________________________________________________________________
#----                  compartment_characteristics class                    ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartment_characteristics",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="compartment_characteristic") 
)

#_______________________________________________________________________________
#----                             getByIndex                                ----
#_______________________________________________________________________________

setMethod("getByIndex", signature=c("compartment_characteristics", "compartment_characteristic"), definition=function(object, x) {
  retValue <- object@list %>% purrr::keep(~(.x@compartment==x@compartment & as.character(class(.x))==as.character(class(x))))
  
  if (length(retValue) > 0) {
    retValue <- retValue[[1]]
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_characteristics"), definition=function(object) {
  for (element in object@list) {
    show(element)
    print("")
  }
})

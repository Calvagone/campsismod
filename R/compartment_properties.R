
#_______________________________________________________________________________
#----                  compartment_properties class                    ----
#_______________________________________________________________________________

#' @export
setClass(
  "compartment_properties",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="compartment_property") 
)

#_______________________________________________________________________________
#----                             getByIndex                                ----
#_______________________________________________________________________________

setMethod("getByIndex", signature=c("compartment_properties", "compartment_property"), definition=function(object, x) {
  retValue <- object@list %>% purrr::keep(~(.x@compartment==x@compartment & as.character(class(.x))==as.character(class(x))))
  
  if (length(retValue) > 0) {
    retValue <- retValue[[1]]
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

setMethod("select", signature=c("compartment_properties"), definition=function(object, ...) {
  args <- list(...)
  types <- c("F", "LAG", "DURATION", "RATE", "INIT")
  msg <- paste0("Please select one of those parameter types: ", paste0(paste0("'", types, "'"), collapse=", "))
  assertthat::assert_that(length(args) > 0, msg=msg)
  type <- args[[1]]
  assertthat::assert_that(type %in% types, msg=msg)
  object@list <- object@list %>% purrr::keep(~.x %>% getRecordName()==type)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_properties"), definition=function(object) {
  for (element in object@list) {
    show(element)
    cat("\n")
  }
})

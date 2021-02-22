
setClass(
  "parameters",
  representation(
    list = "list"
  )
)

#_______________________________________________________________________________
#----                                 filter                                ----
#_______________________________________________________________________________

#' Filter.
#' 
#' @param object generic object
#' @param type parameter type: theta, omega or sigma
#' @return filtered object
#' @export
filter <- function(object, type) {
  stop("No default function is provided")
}

setGeneric("filter", function(object, type) {
  standardGeneric("filter")
})

setMethod("filter", signature=c("parameters", "character"), definition=function(object, type) {
  object@list <- object@list %>% purrr::keep(~as.character(class(.x))==type)
  return(object)
})


#_______________________________________________________________________________
#----                                maxIndex                               ----
#_______________________________________________________________________________

#' Max index.
#' 
#' @param object generic object
#' @param type parameter type: theta, omega or sigma
#' @return filtered object
#' @export
maxIndex <- function(object, type) object

setGeneric("maxIndex", function(object, type) {
  standardGeneric("maxIndex")
})

setMethod("maxIndex", signature=c("parameters", "character"), definition=function(object, type) {
  return((object %>% filter(type=type))@list %>% purrr::map_int(~.x@index) %>% max())
})

#_______________________________________________________________________________
#----                             getParameter                              ----
#_______________________________________________________________________________

#' Get parameter function (single index).
#' 
#' @param object generic object
#' @param type parameter type: theta
#' @param index first index
#' @return filtered object
#' @export
getParameter <- function(object, type, index) {
  stop("No default function is provided")
}

#' Get parameter function (double index).
#' 
#' @param object generic object
#' @param type parameter type: omega or sigma
#' @param index first index
#' @param index2 second index
#' @return filtered object
#' @export
getParameter <- function(object, type, index, index2) {
  stop("No default function is provided")
}

setGeneric("getParameter", function(object, type, index) {
  standardGeneric("getParameter")
})

setGeneric("getParameter", function(object, type, index, index2) {
  standardGeneric("getParameter")
})

setMethod("getParameter", signature=c("parameters", "character", "integer"), definition=function(object, type, index) {
  return(getParameter(object, type=type, index=index, index2=integer()))
})

setMethod("getParameter", signature=c("parameters", "character", "integer", "integer"), definition=function(object, type, index, index2) {
  subList <- object %>% filter(type=type)
  if (type=="theta") {
    parameter <- subList@list %>% purrr::keep(~(.x@index==index))
  } else {
    parameter <- subList@list %>% purrr::keep(~(.x@index==index)&(.x@index2==index2))
  }
  if (length(parameter) >= 1) {
    parameter <- parameter[[1]]
  }
  return(parameter)
})
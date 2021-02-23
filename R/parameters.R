#' Parameters class.
#' 
#' @export
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
#----                                  order                                ----
#_______________________________________________________________________________

#' Order.
#' 
#' @param object generic object
#' @return ordered object
#' @export
order <- function(object) {
  stop("No default function is provided")
}

setGeneric("order", function(object) {
  standardGeneric("order")
})

setMethod("order", signature=c("parameters"), definition=function(object) {
  types <- object@list %>% purrr::map_chr(~as.character(class(.x)))
  indexes1 <- object@list %>% purrr::map_int(~.x@index)
  indexes2 <- object@list %>% purrr::map_int(.f=function(.x){
    if("index2" %in% slotNames(.x)) {
      return(.x@index2)
    } else {
      return(as.integer(0))
    }
  })
  
  # Reorder
  types <- factor(types, levels=c("theta", "omega", "sigma"), labels=c("theta", "omega", "sigma"))
  order <- base::order(types, indexes1, indexes2)
  
  # Apply result to original list
  object@list <- object@list[order]
  return(object)
})

#_______________________________________________________________________________
#----                                 clean                                ----
#_______________________________________________________________________________

#' Clean
#' 
#' @param object generic object
#' @return cleaned object
#' @export
clean <- function(object) {
  stop("No default function is provided")
}

setGeneric("clean", function(object) {
  standardGeneric("clean")
})

setMethod("clean", signature=c("parameters"), definition=function(object) {
  attributes(object@list) <- NULL
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

#_______________________________________________________________________________
#----                         addParameter                                  ----
#_______________________________________________________________________________

#' Add a parameter in list.
#' 
#' @param object generic object
#' @param parameter parameter to add
#' @return filtered object
#' @export
addParameter <- function(object, parameter) {
  stop("No default function is provided")
}

setGeneric("addParameter", function(object, parameter) {
  standardGeneric("addParameter")
})

setMethod("addParameter", signature=c("parameters", "parameter"), definition=function(object, parameter) {
  if (!(object %>% hasParameter(parameter) %>% length() > 0)) {
    object@list <- c(object@list, parameter)
    return(object)
  }
})

#_______________________________________________________________________________
#----                          hasParameter                                 ----
#_______________________________________________________________________________

#' Check if list has given parameter.
#' 
#' @param object generic object
#' @param parameter parameter to check
#' @return the parameter that matches
#' @export
hasParameter <- function(object, parameter) {
  stop("No default function is provided")
}

setGeneric("hasParameter", function(object, parameter) {
  standardGeneric("hasParameter")
})

setMethod("hasParameter", signature=c("parameters", "theta"), definition=function(object, parameter) {
  return(object %>% getParameter(type="theta", index=parameter@index))
})

setMethod("hasParameter", signature=c("parameters", "omega"), definition=function(object, parameter) {
  return(object %>% getParameter(type="omega", index=parameter@index, index2=parameter@index2))
})

setMethod("hasParameter", signature=c("parameters", "sigma"), definition=function(object, parameter) {
  return(object %>% getParameter(type="sigma", index=parameter@index, index2=parameter@index2))
})

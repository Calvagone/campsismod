
#' @export
setClass(
  "parameters",
  representation(
  ),
  contains = "pmx_list"
)

#' 
#' Create a list of parameters.
#' 
#' @return an empty list of parameters  
#' @export
Parameters <- function() {
  return(new("parameters"))
}

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
#----                             fixOmega                                  ----
#_______________________________________________________________________________

#' Fix omega matrix for SAME OMEGA parameters that have NA values due to imperfections in Pharmpy import.
#' 
#' @param object generic object
#' @return the parameter that matches
#' @export
fixOmega <- function(object) {
  stop("No default function is provided")
}

setGeneric("fixOmega", function(object) {
  standardGeneric("fixOmega")
})

setMethod("fixOmega", signature=c("parameters"), definition=function(object) {
  # First order parameters
  tmp <- object %>% order()

  # We need at least to elements
  if (length(tmp@list) < 2) {
    return(object)
  }

  parameters <- new("parameters", list=list())
  
  # Fix NA problems
  # .x is the accumulating value
  # .y is element in the list
  purrr::accumulate(.x=tmp@list, .f=function(.x, .y) {
    xIsOmega <- (class(.x) %>% as.character())=="omega"
    yIsOmega <- (class(.y) %>% as.character())=="omega"
    if (xIsOmega && yIsOmega) {
      if (is.na(.y@value)) {
        .y@value <- .x@value
      }
      if (is.na(.y@fix)) {
        .y@fix <- .x@fix
      }
    }
    parameters <<- parameters %>% addParameter(.y)
    return(.y)
  }, .init=tmp@list[[1]])
  
  return(parameters %>% clean())
})


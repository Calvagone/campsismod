
#_______________________________________________________________________________
#----                          parameters class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "parameters",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="parameter")
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
#----                              disable                                  ----
#_______________________________________________________________________________

setMethod("disable", signature=c("parameters", "character"), definition=function(object, x, ...) {
  msg <- "Only these 2 variabilities can be disabled for now: 'IIV', 'RUV'"
  variabilities <- c("IIV", "RUV")
  assertthat::assert_that(all(x %in% variabilities), msg=msg)
  
  # Disable IIV
  if ("IIV" %in% x) {
    (object%>% select("omega"))@list %>% purrr::map(.f=function(param) {
      param@value <- 0
      object <<- object %>% replace(param)
    })
  }
  # Disable RUV
  if ("RUV" %in% x) {
    (object%>% select("sigma"))@list %>% purrr::map(.f=function(param) {
      param@value <- 0
      object <<- object %>% replace(param)
    })
  }
  return(object)
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
  tmp <- object %>% sort()

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
    parameters <<- parameters %>% add(.y)
    return(.y)
  }, .init=tmp@list[[1]])
  
  return(parameters %>% clean())
})

#_______________________________________________________________________________
#----                             getByIndex                              ----
#_______________________________________________________________________________

#' Get parameter by index (single index).
#' 
#' @param object list of parameters
#' @param parameter to search for
#' @return parameter that matches
#' @export
getByIndex <- function(object, parameter) {
  stop("No default function is provided")
}

setGeneric("getByIndex", function(object, parameter) {
  standardGeneric("getByIndex")
})

setMethod("getByIndex", signature=c("parameters", "parameter"), definition=function(object, parameter) {
  subList <- object %>% select(as.character(class(parameter)))
  if (is(parameter, "theta")) {
    parameter <- subList@list %>% purrr::keep(~(.x@index==parameter@index))
  } else {
    parameter <- subList@list %>% purrr::keep(~(.x@index==parameter@index)&(.x@index2==parameter@index2))
  }
  if (length(parameter) >= 1) {
    parameter <- parameter[[1]]
  }
  return(parameter)
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
  return((object %>% select(type))@list %>% purrr::map_int(~.x@index) %>% max())
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

setMethod("select", signature=c("parameters"), definition=function(object, ...) {
  args <- list(...)
  msg <- "Please select one of those parameter types: 'theta', 'omega' or 'sigma'"
  assertthat::assert_that(length(args) > 0, msg=msg)
  type <- args[[1]]
  assertthat::assert_that(type %in% c("theta", "omega", "sigma"), msg=msg)
  
  object@list <- object@list %>% purrr::keep(~as.character(class(.x))==type)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("parameters"), definition=function(object) {
  thetas <- object %>% select("theta")
  omegas <- object %>% select("omega")
  sigmas <- object %>% select("sigma")
  print("THETA's:")
  print(purrr::map_df(thetas@list, .f=as.data.frame, row.names=character(), optional=FALSE))
  print("OMEGA's:")
  print(purrr::map_df(omegas@list, .f=as.data.frame, row.names=character(), optional=FALSE))
  print("SIGMA's:")
  print(purrr::map_df(sigmas@list, .f=as.data.frame, row.names=character(), optional=FALSE))
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

setMethod("sort", signature=c("parameters"), definition=function(x, decreasing=FALSE, ...) {
  types <- x@list %>% purrr::map_chr(~as.character(class(.x)))
  indexes1 <- x@list %>% purrr::map_int(~.x@index)
  indexes2 <- x@list %>% purrr::map_int(.f=function(.x){
    if("index2" %in% slotNames(.x)) {
      return(.x@index2)
    } else {
      return(as.integer(0))
    }
  })
  
  # Reorder
  types <- factor(types, levels=c("theta", "omega", "sigma"), labels=c("theta", "omega", "sigma"))
  order <- order(types, indexes1, indexes2)
  
  # Apply result to original list
  x@list <- x@list[order]
  return(x)
})

#_______________________________________________________________________________
#----                            standardise                                ----
#_______________________________________________________________________________

setMethod("standardise", signature=c("parameters"), definition=function(object, ...) {
  list <- object@list %>% purrr::map(.f=function(param) {
    return(param %>% standardise())
  })
  retValue <- Parameters()
  retValue@list <- list
  return(retValue)
})

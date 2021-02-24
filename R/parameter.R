
checkParameter <- function(object) {
  errors <- character()
  
  # Optional
  errors <- addError(checkLength(object, "name"), errors)
  
  # Mandatory
  errors <- addError(checkLength(object, "index"), errors)
  errors <- addError(checkLength(object, "fix"), errors)
  errors <- addError(checkLength(object, "value"), errors)

  if (length(errors) == 0) TRUE else errors
}

checkLength <- function(object, slot, expected=1) {
  lengthSlot <- length(eval(parse(text = paste0("object@", slot))))
  error <- NULL
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected)
  }
  return(error)
}

addError <- function(error, errors) {
  if (is.null(error)) {
    return(errors)
  } else {
    return(c(errors, error))
  }
}

#' Parameter class.
#' 
#' @export
setClass(
  "parameter",
  representation(
    name = "character",   # Optional
    index = "integer",    # Mandatory
    value = "numeric",    # Mandatory
    fix = "logical"       # Mandatory
  ),
  validity = checkParameter
)

#' Single array parameter class.
#' 
#' @export
setClass(
  "single_array_parameter",
  representation(
  ),
  validity = checkParameter,
  contains = "parameter"
)

#' Theta parameter class.
#' 
#' @export
setClass(
  "theta",
  representation(
  ),
  validity = checkParameter,
  contains = "single_array_parameter"
)

#' Double array parameter class.
#' 
#' @export
setClass(
  "double_array_parameter",
  representation(
    index2 = "integer"
  ),
  validity = checkParameter,
  contains = "single_array_parameter"
)

#' Omega parameter class.
#' 
#' @export
setClass(
  "omega",
  representation(
  ),
  validity = checkParameter,
  contains = "double_array_parameter"
)

#' Sigma parameter class.
#' 
#' @export
setClass(
  "sigma",
  representation(
  ),
  validity = checkParameter,
  contains = "double_array_parameter"
)

#_______________________________________________________________________________
#----                               isDiag                                  ----
#_______________________________________________________________________________


isDiag <- function(object) TRUE

setGeneric("isDiag", function(object) {
  standardGeneric("isDiag")
})

setMethod("isDiag", signature(object = "double_array_parameter"), function(object) {
  return(object@index==object@index2)
})

#_______________________________________________________________________________
#----                            getNONMEMName                              ----
#_______________________________________________________________________________

#' Get NONMEM name.
#' 
#' @param object generic object
#' @return NONMEM name
#' @export
getNONMEMName <- function(object) {
  stop("No default function is provided")
}

setGeneric("getNONMEMName", function(object) {
  standardGeneric("getNONMEMName")
})

setMethod("getNONMEMName", signature=c("theta"), definition=function(object) {
  return(paste0("THETA(", object@index, ")"))
})

setMethod("getNONMEMName", signature=c("omega"), definition=function(object) {
  return(paste0("OMEGA(", object@index, ",", object@index2, ")"))
})

setMethod("getNONMEMName", signature=c("sigma"), definition=function(object) {
  return(paste0("SIGMA(", object@index, ",", object@index2, ")"))
})

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

getBestName <- function(prefix, name, index) {
  retValue <- ""
  if (length(name)==0 || is.na(name)) {
    retValue <- paste0(prefix, "_", index)
  } else {
    retValue <- paste0(prefix, "_", name)
  }
  return(retValue)
}

setMethod("getName", signature=c("theta"), definition=function(object) {
  return(getBestName("THETA", object@name, object@index))
})

setMethod("getName", signature=c("omega"), definition=function(object) {
  return(getBestName("ETA", object@name, object@index))
})

setMethod("getName", signature=c("sigma"), definition=function(object) {
  return(getBestName("EPS", object@name, object@index))
})


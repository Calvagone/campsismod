
checkParameter <- function(object) {
  errors <- character()
  
  # Mandatory
  errors <- addError(checkLength(object, "index"), errors)
  errors <- addError(checkLength(object, "fix"), errors)
  errors <- addError(checkLength(object, "value"), errors)
  
  # Optional
  errors <- addError(checkLengthOptional(object, "name"), errors)
  errors <- addError(checkLengthOptional(object, "suffix"), errors)
  
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

checkLengthOptional <- function(x, slot, expected=1) {
  error <- NULL
  if (!is.null(checkLength(x, slot, expected=0))) {
    error <- checkLength(x, slot, expected=1)
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
    name = "character",   # Optional, either name is provided or suffix
    index = "integer",    # Mandatory
    suffix = "character", # Optional, either name is provided or suffix
    fix = "logical",      # Mandatory
    value = "numeric"     # Mandatory
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
  return(paste0("OMEGA(", object@index, ",", object@index2, ")"))
})

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

#' Get name in model code.
#' 
#' @param object generic object
#' @return name
#' @export
getName <- function(object) {
  stop("No default function is provided")
}

setGeneric("getName", function(object) {
  standardGeneric("getName")
})

getBestName <- function(prefix, name, suffix, index) {
  retValue <- ""
  if (length(name)==0 || is.na(name)) {
    if (length(suffix)==0 || is.na(suffix)) {
      retValue <- paste0(prefix, "_", index)
    } else {
      retValue <- paste0(prefix, "_", suffix)
    }
  } else {
    retValue <- name
  }
  return(retValue)
}

setMethod("getName", signature=c("theta"), definition=function(object) {
  return(getBestName("THETA", object@name, object@suffix, object@index))
})

setMethod("getName", signature=c("omega"), definition=function(object) {
  return(getBestName("ETA", object@name, object@suffix, object@index))
})

setMethod("getName", signature=c("sigma"), definition=function(object) {
  return(getBestName("EPS", object@name, object@suffix, object@index))
})


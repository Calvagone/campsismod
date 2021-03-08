
checkParameter <- function(object) {
  check1 <- expectOneForAll(object, c("name", "index", "fix", "value"))
  check2 <- if (is.na(object@index)) {"Index can't be NA"} else {character()} 
  return(c(check1, check2))
}

#' @export
setClass(
  "parameter",
  representation(
    name = "character",
    index = "integer",
    value = "numeric",
    fix = "logical"
  ),
  contains = "pmx_element",
  prototype = prototype(name=as.character(NA), value=as.numeric(NA), fix=FALSE),
  validity = checkParameter
)

#' @export
setClass(
  "single_array_parameter",
  representation(
  ),
  contains = "parameter"
)

checkDoubleArrayParameter <- function(object) {
  check <- if (is.na(object@index2)) {"Index2 can't be NA"} else {character()} 
  return(check)
}

#' @export
setClass(
  "double_array_parameter",
  representation(
    index2 = "integer"
  ),
  contains = "single_array_parameter",
  validity = checkDoubleArrayParameter
)

#_______________________________________________________________________________
#----                                theta                                  ----
#_______________________________________________________________________________


#' Theta parameter class.
#' 
#' @export
setClass(
  "theta",
  representation(
  ),
  contains = "single_array_parameter"
)

#' 
#' Create a THETA parameter.
#' 
#' @param name parameter name, e.g. CL (prefix THETA will be added automatically)
#' @param index parameter index
#' @param value parameter value
#' @param fix parameter was fixed in estimation, logical value
#' @return a THETA parameter  
#' @export
Theta <- function(name=NA, index, value=NA, fix=FALSE) {
  return(new("theta", name=as.character(name), index=as.integer(index), value=as.numeric(value), fix=fix))
}

#_______________________________________________________________________________
#----                                omega                                  ----
#_______________________________________________________________________________

#' Omega parameter class.
#' 
#' @export
setClass(
  "omega",
  representation(
  ),
  contains = "double_array_parameter"
)

#' 
#' Create an OMEGA parameter.
#' 
#' @param name parameter name, e.g. CL (prefix OMEGA will be added automatically)
#' @param index parameter index
#' @param index2 second parameter index
#' @param value parameter value
#' @param fix parameter was fixed in estimation, logical value
#' @return an OMEGA parameter  
#' @export
Omega <- function(name=NA, index, index2, value=NA, fix=FALSE) {
  return(new("omega", name=as.character(name), index=as.integer(index), index2=as.integer(index2), value=as.numeric(value), fix=fix))
}

#_______________________________________________________________________________
#----                                sigma                                  ----
#_______________________________________________________________________________

#' Sigma parameter class.
#' 
#' @export
setClass(
  "sigma",
  representation(
  ),
  contains = "double_array_parameter"
)

#' 
#' Create a SIGMA parameter.
#' 
#' @param name parameter name, e.g. CL (prefix SIGMA will be added automatically)
#' @param index parameter index
#' @param index2 second parameter index
#' @param value parameter value
#' @param fix parameter was fixed in estimation, logical value
#' @return a SIGMA parameter  
#' @export
Sigma <- function(name=NA, index, index2, value=NA, fix=FALSE) {
  return(new("sigma", name=as.character(name), index=as.integer(index), index2=as.integer(index2), value=as.numeric(value), fix=fix))
}

#_______________________________________________________________________________
#----                               isDiag                                  ----
#_______________________________________________________________________________

#' Is diagonal.
#' 
#' @param object generic object
#' @return logical value
#' @export
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

setMethod("getName", signature=c("theta"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("THETA", "_", x@index))
  } else {
    return(paste0("THETA", "_", x@name))
  }
})

setMethod("getName", signature=c("omega"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("OMEGA", "_", x@index, "_", x@index2))
  } else {
    return(paste0("OMEGA", "_", x@name))
  }
})

setMethod("getName", signature=c("sigma"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("SIGMA", "_", x@index, "_", x@index2))
  } else {
    return(paste0("SIGMA", "_", x@name))
  }
})

#_______________________________________________________________________________
#----                         getNameInModel                                ----
#_______________________________________________________________________________

#' Get name of parameter in the PMX model.
#' 
#' @param x element to know the name
#' @return the name of this element
#' @export
getNameInModel <- function(x) {
  stop("No default function is provided")
}

setGeneric("getNameInModel", function(x) {
  standardGeneric("getNameInModel")
})

setMethod("getNameInModel", signature=c("theta"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("THETA", "_", x@index))
  } else {
    return(paste0("THETA", "_", x@name))
  }
})

setMethod("getNameInModel", signature=c("omega"), definition=function(x) {
  if (is.na(x@name)) {
    if (x@index != x@index2) {
      stop("You should not call this method with different indexes!")
    }
    return(paste0("ETA", "_", x@index))
  } else {
    return(paste0("ETA", "_", x@name))
  }
})

setMethod("getNameInModel", signature=c("sigma"), definition=function(x) {
  if (is.na(x@name)) {
    if (x@index != x@index2) {
      stop("You should not call this method with different indexes!")
    }
    return(paste0("EPS", "_", x@index))
  } else {
    return(paste0("EPS", "_", x@name))
  }
})


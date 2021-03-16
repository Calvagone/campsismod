
#_______________________________________________________________________________
#----                          parameter class                              ----
#_______________________________________________________________________________

validateParameter <- function(object) {
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
  validity = validateParameter
)

#_______________________________________________________________________________
#----                   single_array_parameter class                        ----
#_______________________________________________________________________________


#' @export
setClass(
  "single_array_parameter",
  representation(
  ),
  contains = "parameter"
)

#_______________________________________________________________________________
#----                   double_array_parameter class                        ----
#_______________________________________________________________________________

validateDoubleArrayParameter <- function(object) {
  check1 <- if (is.na(object@index2)) {"Index2 can't be NA"} else {character()}
  check2 <- expectOne(object, "type")
  check3 <-
    if (object@type %in% c("var", "sd", "covar", "cv", "cv%")) {
      character()
    } else {
      "Type should be one of: 'var', 'sd', 'covar', 'cv' or 'cv%'"
    }
  check4 <- 
    if (object@index != object@index2 && !(object@type %in% c("covar"))) {
      paste0("Parameter type must be 'covar' (index:", object@index, ", index2:", object@index2, ")")
    } else {
      character()
    }
  check5 <- 
    if (object@index == object@index2 && object@type %in% c("covar")) {
      paste0("Parameter type can't be 'covar' (index:", object@index, ", index2:", object@index2, ")")
    } else {
      character()
    }
  return(c(check1, check2, check3, check4, check5))
}

#' @export
setClass(
  "double_array_parameter",
  representation(
    index2 = "integer",
    type = "character"
  ),
  contains = "single_array_parameter",
  prototype = prototype(type="var"),
  validity = validateDoubleArrayParameter
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
#' @param type variance type: 'var', 'sd', 'covar', 'cv' or 'cv\%'
#' @return an OMEGA parameter  
#' @export
Omega <- function(name=NA, index, index2, value=NA, fix=FALSE, type=NULL) {
  if (is.null(type)) {
    type <- if (index==index2) {"var"} else {"covar"}
  }
  return(new("omega", name=as.character(name), index=as.integer(index), index2=as.integer(index2),
             value=as.numeric(value), fix=fix, type=type))
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
#' @param type variance type: 'var', 'sd', 'covar', 'cv' or 'cv\%'
#' @return a SIGMA parameter  
#' @export
Sigma <- function(name=NA, index, index2, value=NA, fix=FALSE, type=NULL) {
  if (is.null(type)) {
    type <- if (index==index2) {"var"} else {"covar"}
  }
  return(new("sigma", name=as.character(name), index=as.integer(index), index2=as.integer(index2),
             value=as.numeric(value), fix=fix, type=type))
}

#_______________________________________________________________________________
#----                           as.data.frame                               ----
#_______________________________________________________________________________

#' As data frame method.
#'
#' @param x generic object
#' @param row.names row names
#' @param optional optional
#' @param ... extra arguments
#' @return data frame
#' @export
as.data.frame <- function(x, row.names=NULL, optional=FALSE, ...) {
  base::as.data.frame(x, row.names=row.names, optional=optional, ...)
}

setGeneric("as.data.frame", function(x, row.names=NULL, optional=FALSE, ...) {
  standardGeneric("as.data.frame")
})

setMethod("as.data.frame", signature("theta", "character", "logical"), function(x, row.names=NULL, optional=FALSE, ...) {
  return(data.frame(name=x@name, index=x@index, value=x@value, fix=x@fix))
})

setMethod("as.data.frame", signature("double_array_parameter", "character", "logical"), function(x, row.names=NULL, optional=FALSE, ...) {
  return(data.frame(name=x@name, index=x@index, index2=x@index2, value=x@value, fix=x@fix, type=x@type))
})

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

#_______________________________________________________________________________
#----                            standardise                                ----
#_______________________________________________________________________________

setMethod("standardise", signature=c("theta"), definition=function(object, ...) {
  return(object)
})

setMethod("standardise", signature=c("double_array_parameter"), definition=function(object, ...) {
  type <- object@type
  index <- object@index
  index2 <- object@index2
  retValue <- object# Copy

  if (index == index2) {
    if (type == "var") {
      # Do nothing
    
    } else if (type == "sd") {
      retValue@value <- object@value ^ 2
    
    } else if (type == "covar") {
      stop(paste0("Type of parameter ", x %>% getName(), " can't be 'covar'"))
    
    } else if (type == "cv") {
      retValue@value <- log(object@value^2+1)
    
    } else if (type == "cv%") {
      retValue@value <- log((object@value/100)^2+1)
    } else {
      stop("Type should be one of: 'var', 'sd', 'covar', 'cv' or 'cv%'")
    }
    retValue@type <- "var"
  } else {
    if (type == "covar") {
      # Do nothing
    } else {
      stop(paste0("Type of parameter ", object@value %>% getName(), " must be 'covar'"))
    }
    retValue@type <- "covar"
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("parameter"), definition=function(object) {
  print(object %>% as.data.frame(row.names=character(), optional=FALSE))
})

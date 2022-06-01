
#_______________________________________________________________________________
#----                          parameter class                              ----
#_______________________________________________________________________________

#' 
#' Parameter class. Any parameter in a pharmacometric model.
#' 
#' @slot name parameter name, optional (although recommended)
#' @slot index parameter index, integer
#' @slot value parameter value (e.g. the estimated value from a modelling tool)
#' @slot fix logical value, say if parameter was fixed in the modelling phase
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
  prototype = prototype(name=as.character(NA), index=as.integer(NA), value=as.numeric(NA), fix=FALSE),
  validity = function(object) {
    check <- expectOneForAll(object, c("name", "index", "fix", "value"))
    return(check)
  }
)

#_______________________________________________________________________________
#----                   single_array_parameter class                        ----
#_______________________________________________________________________________

#' 
#' Single-array parameter class. This parameter has a single index value.
#' 
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

#' 
#' Double-array parameter class. This parameter has 2 indexes. 
#' It can thus be used to define correlations between parameters.
#' 
#' @export
setClass(
  "double_array_parameter",
  representation(
    index2 = "integer",
    type = "character"
  ),
  contains = "single_array_parameter",
  prototype = prototype(type="var"),
  validity = function(object) {
    check1 <- expectOne(object, "type")
    check2 <-
      if (object@type %in% c("var", "sd", "covar", "cor", "cv", "cv%")) {
        character()
      } else {
        "Type should be one of: 'var', 'sd', 'covar', 'cor', 'cv' or 'cv%'"
      }
    check3 <- 
      if (is.na(object@index) && is.na(object@index2)) {
        character() # Don't go further
      } else if (object@index != object@index2 && !(object@type %in% c("covar", "cor"))) {
        paste0("Parameter type must be 'covar' or 'cor' (index:", object@index, ", index2:", object@index2, ")")
      } else {
        character()
      }
    check4 <- 
      if (is.na(object@index) && is.na(object@index2)) {
        character() # Don't go further
      } else if (object@index == object@index2 && object@type %in% c("covar", "cor")) {
        paste0("Parameter type can't be 'covar' nor 'cor' (index:", object@index, ", index2:", object@index2, ")")
      } else {
        character()
      }
    return(c(check1, check2, check3, check4))
  }
)

#_______________________________________________________________________________
#----                                theta                                  ----
#_______________________________________________________________________________

#' 
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
Theta <- function(name=NA, index=NA, value=NA, fix=FALSE) {
  return(new("theta", name=as.character(name), index=as.integer(index), value=as.numeric(value), fix=fix))
}

#_______________________________________________________________________________
#----                                omega                                  ----
#_______________________________________________________________________________

#'
#' Omega parameter class.
#' 
#' @slot same logical value, tell if this omega is the same as the previous one
#' @export
setClass(
  "omega",
  representation(
    same = "logical"
  ),
  contains = "double_array_parameter",
  prototype = prototype(same=as.logical(NA), index2=as.integer(NA)),
  validity = function(object) {
    return(expectOne(object, "same"))
  }
)

#' 
#' Process double array arguments.
#' 
#' @param index parameter index
#' @param index2 second parameter index
#' @param type variance type
#' @return variance type
#' @keywords internal
#'
processDoubleArrayArguments <- function(index, index2, type) {
  # Only 1 NA: NOK
  if (xor(is.na(index), is.na(index2))) {
    stop("Please specify both OMEGA indexes")
  }
  # Both NA: OK
  if (is.null(type) && is.na(index) && is.na(index2)) {
    type <- "var"
  }
  if (is.null(type)) {
    type <- if (index==index2) {"var"} else {"covar"}
  }
  return(type)
}

#' 
#' Create an OMEGA parameter.
#' 
#' @param name parameter name, e.g. CL (prefix OMEGA will be added automatically)
#' @param index parameter index
#' @param index2 second parameter index
#' @param value parameter value
#' @param fix parameter was fixed in estimation, logical value
#' @param type variance type: 'var', 'sd', 'covar', 'cor', 'cv' or 'cv\%'
#' @param same NA by default, FALSE for first OMEGA followed by 'SAME' OMEGA's, TRUE for 'SAME' OMEGA's
#' @return an OMEGA parameter  
#' @export
Omega <- function(name=NA, index=NA, index2=NA, value=NA, fix=FALSE, type=NULL, same=NA) {
  type <- processDoubleArrayArguments(index=index, index2=index2, type=type)
  return(new("omega", name=as.character(name), index=as.integer(index), index2=as.integer(index2),
             value=as.numeric(value), fix=fix, type=type, same=as.logical(same)))
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
Sigma <- function(name=NA, index=NA, index2=NA, value=NA, fix=FALSE, type=NULL) {
  type <- processDoubleArrayArguments(index=index, index2=index2, type=type)
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
#' @rdname as.data.frame
as.data.frame <- function(x, row.names=NULL, optional=FALSE, ...) {
  base::as.data.frame(x, row.names=row.names, optional=optional, ...)
}

setGeneric("as.data.frame", function(x, row.names=NULL, optional=FALSE, ...) {
  standardGeneric("as.data.frame")
})

#' @rdname as.data.frame
setMethod("as.data.frame", signature("theta", "character", "logical"), function(x, row.names=NULL, optional=FALSE, ...) {
  return(data.frame(name=x@name, index=x@index, value=x@value, fix=x@fix))
})

#' @rdname as.data.frame
setMethod("as.data.frame", signature("omega", "character", "logical"), function(x, row.names=NULL, optional=FALSE, ...) {
  return(data.frame(name=x@name, index=x@index, index2=x@index2, value=x@value, fix=x@fix, type=x@type, same=x@same))
})

#' @rdname as.data.frame
setMethod("as.data.frame", signature("sigma", "character", "logical"), function(x, row.names=NULL, optional=FALSE, ...) {
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
#' @rdname isDiag
isDiag <- function(object) TRUE

setGeneric("isDiag", function(object) {
  standardGeneric("isDiag")
})

#' @rdname isDiag
setMethod("isDiag", signature(object = "double_array_parameter"), function(object) {
  return(object@index==object@index2)
})

#_______________________________________________________________________________
#----                            getNONMEMName                              ----
#_______________________________________________________________________________

#' Get NONMEM name.
#' 
#' @param object generic object
#' @return the NONMEM name associated with this object
#' @export
#' @rdname getNONMEMName
getNONMEMName <- function(object) {
  stop("No default function is provided")
}

setGeneric("getNONMEMName", function(object) {
  standardGeneric("getNONMEMName")
})

#' @rdname getNONMEMName
setMethod("getNONMEMName", signature=c("theta"), definition=function(object) {
  return(paste0("THETA(", object@index, ")"))
})

#' @rdname getNONMEMName
setMethod("getNONMEMName", signature=c("omega"), definition=function(object) {
  return(paste0("OMEGA(", object@index, ",", object@index2, ")"))
})

#' @rdname getNONMEMName
setMethod("getNONMEMName", signature=c("sigma"), definition=function(object) {
  return(paste0("SIGMA(", object@index, ",", object@index2, ")"))
})

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature=c("theta"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("THETA", "_", x@index))
  } else {
    return(paste0("THETA", "_", x@name))
  }
})

#' @rdname getName
setMethod("getName", signature=c("omega"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("OMEGA", "_", x@index, "_", x@index2))
  } else {
    return(paste0("OMEGA", "_", x@name))
  }
})

#' @rdname getName
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

#' Get the name of the given parameter in the CAMPSIS model.
#' 
#' @param x element to know the name
#' @return the name of this parameter
#' @export
#' @rdname getNameInModel 
getNameInModel <- function(x) {
  stop("No default function is provided")
}

setGeneric("getNameInModel", function(x) {
  standardGeneric("getNameInModel")
})

#' @rdname getNameInModel 
setMethod("getNameInModel", signature=c("theta"), definition=function(x) {
  if (is.na(x@name)) {
    return(paste0("THETA", "_", x@index))
  } else {
    return(paste0("THETA", "_", x@name))
  }
})

#' @rdname getNameInModel 
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

#' @rdname getNameInModel 
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
#----                           getUncertainty                              ----
#_______________________________________________________________________________

#' @importFrom tibble tibble
#' @rdname getUncertainty
setMethod("getUncertainty", signature=c("parameter"), definition=function(object, varcov, ...) {
  name <- object %>% getName()
  if (varcov %>% length() > 0) {
    standardisedParameter <- object %>% standardise()
    if (name %in% colnames(varcov)) {
      variance <- varcov[name, name]
      return(tibble::tibble(name=name, se=sqrt(variance), "rse%"=100*sqrt(variance)/abs(standardisedParameter@value)))
    }
  }
  return(tibble::tibble(name=name, se=as.numeric(NA), "rse%"=as.numeric(NA)))
})

#_______________________________________________________________________________
#----                            standardise                                ----
#_______________________________________________________________________________

#' @rdname standardise
setMethod("standardise", signature=c("theta"), definition=function(object, ...) {
  return(object)
})

#' @rdname standardise
#' @param parameters the list of parameters, to be provided only if parameter type is 'cor'
setMethod("standardise", signature=c("double_array_parameter"), definition=function(object, parameters=NULL, ...) {
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
      stop(paste0("Type of parameter ", object %>% getName(), " can't be 'covar'"))
    
    } else if (type == "cv") {
      retValue@value <- log(object@value^2+1)
    
    } else if (type == "cv%") {
      retValue@value <- log((object@value/100)^2+1)
    } else {
      stop("Type should be one of: 'var', 'sd', 'cv' or 'cv%'")
    }
    retValue@type <- "var"
  } else {
    if (type == "covar") {
      # Do nothing
      
    } else if (type == "cor") {
      if (is.null(parameters)) {
        stop("Argument 'parameters' is needed to convert a covariance into a correlation")
      }
      # Retrieve both omega's on the diagonal related to index and index2
      # Make sure to standardise them to variances first
      omega1 <- parameters %>% getByIndex(Omega(index=object@index, index2=object@index)) %>% standardise()
      omega2 <- parameters %>% getByIndex(Omega(index=object@index2, index2=object@index2)) %>% standardise()
      retValue@value <- object@value*sqrt(omega1@value)*sqrt(omega2@value)
    } else {
      stop(paste0("Type of parameter ", object %>% getName(), " must be 'covar' or 'cor'"))
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

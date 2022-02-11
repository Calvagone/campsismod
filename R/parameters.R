
#_______________________________________________________________________________
#----                          parameters class                             ----
#_______________________________________________________________________________

validateParametersByType <- function(object, type, emptyParameter) {
  params <- object %>% select(type)
  if (params %>% length() == 0) {
    return(character())
  }
  maxIndex <- params %>% maxIndex()
  minIndex <- params %>% minIndex()
  if (is.na(minIndex)) {
    return(paste0("At least one ", type %>% toupper(), " index is NA"))
  }
  if (minIndex != 1) {
    return(paste0("First ", type %>% toupper(), " index is different than 1"))
  }
  for (i in seq_len(maxIndex)) {
    search <- emptyParameter
    if (is(emptyParameter, "double_array_parameter")) {
      search@index <- i
      search@index2 <- i
    } else {
      search@index <- i
    }
    param <- params %>% getByIndex(search)
    if (length(param) == 0) {
      return(paste0("No ", type %>% toupper(), " with index ", i))
    }
    if (is.na(param@value)) {
      return(paste0(type %>% toupper(), " with index ", i, " has NA value"))
    }
  }
  return(character())
}

validateParameters <- function(object) {
  check1 <- validateParametersByType(object, "theta", Theta())
  check2 <- validateParametersByType(object, "omega", Omega())
  check3 <- validateParametersByType(object, "sigma", Sigma())
  return(c(check1, check2, check3))
}

#' 
#' Parameters class.
#' 
#' @slot varcov associated variance-covariance matrix
#' @export
setClass(
  "parameters",
  representation(
    varcov = "matrix"
  ),
  contains = "pmx_list",
  prototype = prototype(type="parameter", varcov=matrix(numeric(0), nrow=0, ncol=0)),
  validity = validateParameters
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
#----                              add                                      ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature=c("parameters", "single_array_parameter"), definition=function(object, x) {
  if (is.na(x@index)) {
    maxIndex <- object %>% select(as.character(class(x))) %>% maxIndex()
    x@index <- as.integer(maxIndex + 1)
  }
  return(methods::callNextMethod(object, x))
})

#' @rdname add
setMethod("add", signature=c("parameters", "double_array_parameter"), definition=function(object, x) {
  if (is.na(x@index) && is.na(x@index2)) {
    maxIndex <- object %>% select(as.character(class(x))) %>% maxIndex()
    x@index <- as.integer(maxIndex + 1)
    x@index2 <- as.integer(maxIndex + 1)
  }
  return(methods::callNextMethod(object, x))
})

#' @rdname add
setMethod("add", signature=c("parameters", "parameters"), definition=function(object, x) {
  return(object %>% appendParameters(x))
})

#' Append parameters.
#' 
#' @param params1 base set of parameters
#' @param params2 extra set of parameters to be appended
#' @return the resulting set of parameters
#' @keywords internal
#' 
appendParameters <- function(params1, params2) {
  paramNames1 <- params1 %>% getNames()
  paramNames2 <- params2 %>% getNames()
  
  checkCollisionOnParams <- paramNames1 %in% paramNames2
  if (any(checkCollisionOnParams)) {
    stop(paste0("Model can't be appended because of duplicate parameter name(s): ", paste0(paramNames1[checkCollisionOnParams], collapse=", ")))
  }
  
  thetaMax <- params1 %>% select("theta") %>% maxIndex()
  omegaMax <- params1 %>% select("omega") %>% maxIndex()
  sigmaMax <- params1 %>% select("sigma") %>% maxIndex()

  for (theta in (params2 %>% select("theta"))@list) {
    theta@index <- theta@index + thetaMax
    params1 <- params1 %>% add(theta)
  }
  for (omega in (params2 %>% select("omega"))@list) {
    omega@index <- omega@index + omegaMax
    omega@index2 <- omega@index2 + omegaMax
    params1 <- params1 %>% add(omega)
  }
  for (sigma in (params2 %>% select("sigma"))@list) {
    sigma@index <- sigma@index + sigmaMax
    sigma@index2 <- sigma@index2 + sigmaMax
    params1 <- params1 %>% add(sigma)
  }
  return(params1 %>% sort())
}

#_______________________________________________________________________________
#----                                 clean                                ----
#_______________________________________________________________________________

#' Clean
#' 
#' @param object generic object
#' @return cleaned object
#' @export
#' @rdname clean
clean <- function(object) {
  stop("No default function is provided")
}

setGeneric("clean", function(object) {
  standardGeneric("clean")
})

#' @rdname clean
setMethod("clean", signature=c("parameters"), definition=function(object) {
  attributes(object@list) <- NULL
  return(object)
})

#_______________________________________________________________________________
#----                             delete                                    ----
#_______________________________________________________________________________

#' Match single array parameter from list based on the name instead of the index.
#' If a match is found, its index is automatically copied.
#' 
#' @param object parameters
#' @param x single array parameter to match
#' @return the same parameter is no match was found or the same parameter with updated index if a match was found
#' @keywords internal
matchSingleArrayParameter <- function(object, x) {
  # If index is NA, index will be the index of the replaced parameter
  if (is.na(x@index) && !is.na(x@name)) {
    existingParam <- object %>% getByName(x %>% getName())
    if (existingParam %>% length() == 1) {
      x@index <- existingParam@index   # Copy index!
    }
  }
  return(x)
}

#' Match double array parameter from list based on the name instead of the index.
#' If a match is found, its indexes are automatically copied.
#' 
#' @param object parameters
#' @param x double array parameter to match
#' @return the same parameter is no match was found or the same parameter with updated indexes if a match was found
#' @keywords internal
matchDoubleArrayParameter <- function(object, x) {
  # If index is NA, index will be the index of the replaced parameter
  if (is.na(x@index) && is.na(x@index2) && !is.na(x@name)) {
    existingParam <- object %>% getByName(x %>% getName())
    if (existingParam %>% length() == 1) {
      x@index <- existingParam@index   # Copy index!
      x@index2 <- existingParam@index2 # Copy index2!
    }
  }
  return(x)
}

#' @rdname delete
setMethod("delete", signature=c("parameters", "single_array_parameter"), definition=function(object, x) {
  x <- matchSingleArrayParameter(object, x)
  return(methods::callNextMethod(object, x))
})

#' @rdname delete
setMethod("delete", signature=c("parameters", "double_array_parameter"), definition=function(object, x) {
  x <- matchDoubleArrayParameter(object, x)
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' @rdname disable
setMethod("disable", signature=c("parameters", "character"), definition=function(object, x, ...) {
  variabilities <- c("IIV", "IOV", "RUV", "VARCOV", "VARCOV_OMEGA", "VARCOV_SIGMA")
  msg <- paste0("Only these variabilities can be disabled: ", paste0("'", variabilities, "'", collapse=", "))
  assertthat::assert_that(list(...) %>% length()==0, msg="Extra arguments are not accepted")
  assertthat::assert_that(all(x %in% variabilities), msg=msg)
  
  # Disable IIV
  if ("IIV" %in% x) {
    (object%>% select("omega"))@list %>% purrr::map(.f=function(param) {
      param@value <- 0
      object <<- object %>% replace(param)
    })
  }
  
  # Disable IOV (note that IOV is a subset of IIV)
  if ("IOV" %in% x) {
    (object%>% select("omega"))@list %>% purrr::map(.f=function(param) {
      if (!is.na(param@same)) {
        param@value <- 0
        object <<- object %>% replace(param)
      }
    })
  }
  
  # Disable RUV
  if ("RUV" %in% x) {
    (object%>% select("sigma"))@list %>% purrr::map(.f=function(param) {
      param@value <- 0
      object <<- object %>% replace(param)
    })
  }
  
  # Disable VARCOV (variance covariance matrix)
  if ("VARCOV" %in% x) {
    object@varcov <- matrix(numeric(0), ncol=0, nrow=0)
  }
  
  # Disable all omegas or sigmas in varcov
  varcovOmega <- "VARCOV_OMEGA" %in% x
  varcovSigma <- "VARCOV_SIGMA" %in% x
  if (varcovOmega || varcovSigma) {

    # Retrieve varcov parameters
    varcovParams <- colnames(object@varcov) %>% purrr::map(.f=function(.x) {
      return(object %>% getByName(.x))
    })
    
    # Remove these params if condition is met
    varcovParams %>% purrr::map(.f=function(.x) {
      if ((is(.x, "omega") && varcovOmega) || (is(.x, "sigma") && varcovSigma)) {
        index <- which(colnames(object@varcov) == .x %>% getName())
        object@varcov <<- object@varcov[-index, ]
        object@varcov <<- object@varcov[, -index]
      }
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
#' @rdname fixOmega
fixOmega <- function(object) {
  stop("No default function is provided")
}

setGeneric("fixOmega", function(object) {
  standardGeneric("fixOmega")
})

#' @rdname fixOmega
setMethod("fixOmega", signature=c("parameters"), definition=function(object) {
  # First order parameters
  tmp <- object %>% sort()

  # We need at least to elements
  if (length(tmp@list) < 2) {
    return(object)
  }
  
  # Checking all 'same' are NA's
  sameVector <- (tmp %>% select("omega"))@list %>% purrr::map_lgl(.f=~.x@same)
  assertthat::assert_that(all(is.na(sameVector)), msg="all 'same' must be NA")

    # Copy object and clear list of parameters
  parameters <- Parameters()
  
  # Fix NA problems
  # .x is the accumulating value
  # .y is element in the list
  purrr::accumulate(.x=tmp@list, .f=function(.x, .y) {
    xIsOmega <- (class(.x) %>% as.character())=="omega"
    yIsOmega <- (class(.y) %>% as.character())=="omega"
    if (xIsOmega && yIsOmega) {
      if (is.na(.y@value)) {
        .y@value <- .x@value
        .y@same <- TRUE
        if (is.na(.x@same)) {
          .x@same <- FALSE
          parameters <<- parameters %>% replace(.x)
        }
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

#' @rdname getByIndex
setMethod("getByIndex", signature=c("parameters", "parameter"), definition=function(object, x) {
  subList <- object %>% select(as.character(class(x)))
  if (is(x, "theta")) {
    retValue <- subList@list %>% purrr::keep(~(.x@index==x@index))
  } else {
    retValue <- subList@list %>% purrr::keep(~(.x@index==x@index)&(.x@index2==x@index2))
  }
  if (length(retValue) > 0) {
    retValue <- retValue[[1]]
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                                minIndex                               ----
#_______________________________________________________________________________

#' Min index.
#' 
#' @param object generic object
#' @return min index
#' @export
#' @rdname minIndex
minIndex <- function(object) {
  stop("No default function is provided")
}

setGeneric("minIndex", function(object) {
  standardGeneric("minIndex")
})

#' @rdname minIndex
setMethod("minIndex", signature=c("parameters"), definition=function(object) {
  if (object %>% length() == 0) {
    return(0)
  }
  return(object@list %>% purrr::map_int(.f=function(.x) {
    if (is(.x, "double_array_parameter")) {
      return(min(c(.x@index, .x@index2)))
    } else {
      return(.x@index)
    }
  }) %>% min())
})

#_______________________________________________________________________________
#----                                maxIndex                               ----
#_______________________________________________________________________________

#' Max index.
#' 
#' @param object generic object
#' @return max index
#' @export
#' @rdname maxIndex
maxIndex <- function(object) {
  stop("No default function is provided")
}

setGeneric("maxIndex", function(object) {
  standardGeneric("maxIndex")
})

#' @rdname maxIndex
setMethod("maxIndex", signature=c("parameters"), definition=function(object) {
  if (object %>% length() == 0) {
    return(as.integer(0))
  }
  return(object@list %>% purrr::map_int(.f=function(.x) {
    if (is(.x, "double_array_parameter")) {
      return(max(c(.x@index, .x@index2)))
    } else {
      return(.x@index)
    }
  }) %>% max())
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

dataframeToParameter <- function(row, type) {
  param <- NULL
  name <- ifelse(is.null(row$name), NA, row$name) # Optional
  if (type=="theta") {
    param <- Theta(name=name, index=row$index, value=row$value, fix=row$fix)
  } else if(type=="omega") {
    same <- ifelse(is.null(row$same), NA, row$same) # Optional
    param <- Omega(name=name, index=row$index, index2=row$index2, value=row$value, fix=row$fix, type=row$type, same=same)
  } else if(type=="sigma") {
    param <- Sigma(name=name, index=row$index, index2=row$index2, value=row$value, fix=row$fix, type=row$type)
  } else {
    stop(paste0("type must be one of: theta, omega or sigma"))
  }
  return(param)
}

#' Read parameters file.
#' 
#' @param file path to CSV file
#' @param type parameter type: 'theta', 'omega' or 'sigma'
#' @return parameters sub list
#' @importFrom readr read_delim
#' @export
read.parameters <- function(file, type) {
  assertthat::assert_that(type %in% c("theta", "omega", "sigma"),
                          msg="Type must be one of these: 'theta', 'omega' or 'sigma'")
  df <- readr::read_delim(file=file, lazy=FALSE, show_col_types=FALSE, progress=FALSE) %>%
    dplyr::mutate(ROWID=dplyr::row_number())
  list <- df %>% plyr::dlply(.variables="ROWID", .fun=dataframeToParameter, type=type)
  attributes(list) <- NULL
  return(new("parameters", list=list))
}

#' Read variance-covariance file.
#' 
#' @param file path to CSV file
#' @return variance-covariance matrix
#' @importFrom assertthat assert_that
#' @importFrom utils read.csv
#' @export
read.varcov <- function(file) {
  dataframe <- utils::read.csv(file=file)
  row.names(dataframe) <- dataframe[,1] # First column contains parameter names
  matrix <- dataframe[,-1] %>% as.matrix()
  assertthat::assert_that(all(rownames(matrix)==colnames(matrix)), 
      msg="Row names are different than column names in variance-covariance matrix")
  return(matrix)
}

#' Read all parameters files at once.
#' 
#' @param folder path to folder or path to zipped project
#' @return parameters object
#' @export
read.allparameters <- function(folder) {
  thetaPath <- file.path(folder, "theta.csv")
  omegaPath <- file.path(folder, "omega.csv")
  sigmaPath <- file.path(folder, "sigma.csv")
  varcovPath <- file.path(folder, "varcov.csv")
  
  if (file.exists(thetaPath)) {
    theta <- read.parameters(file=thetaPath, type="theta")
  } else {
    theta <- Parameters()
    warning(paste0("No file 'theta.csv' could be found."))
  }
  if (file.exists(omegaPath)) {
    omega <- read.parameters(file=omegaPath, type="omega")
  } else {
    omega <- Parameters()
    warning(paste0("No file 'omega.csv' could be found."))
  }
  if (file.exists(sigmaPath)) {
    sigma <- read.parameters(file=sigmaPath, type="sigma")
  } else {
    sigma <- Parameters()
    warning(paste0("No file 'sigma.csv' could be found."))
  }

  parameters <- new("parameters", list=c(theta@list, omega@list, sigma@list)) %>% clean()
  if (file.exists(varcovPath)) {
    varcov <- read.varcov(varcovPath)
    parameters@varcov <- varcov
  }
  return(parameters)
}

#_______________________________________________________________________________
#----                             replace                                   ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature=c("parameters", "single_array_parameter"), definition=function(object, x) {
  x <- matchSingleArrayParameter(object, x)
  return(methods::callNextMethod(object, x))
})

#' @rdname replace
setMethod("replace", signature=c("parameters", "double_array_parameter"), definition=function(object, x) {
  x <- matchDoubleArrayParameter(object, x)
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

#' @rdname select
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
  cat("THETA's:\n")
  print(purrr::map_df(thetas@list, .f=as.data.frame, row.names=character(), optional=FALSE))
  cat("OMEGA's:\n")
  print(purrr::map_df(omegas@list, .f=as.data.frame, row.names=character(), optional=FALSE))
  cat("SIGMA's:\n")
  print(purrr::map_df(sigmas@list, .f=as.data.frame, row.names=character(), optional=FALSE))
  
  if (object@varcov %>% length() == 0) {
    cat("No variance-covariance matrix\n")
  } else {
    cat("Variance-covariance matrix:\n")
    show(object@varcov)
  }
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

#' @rdname sort
setMethod("sort", signature=c("parameters"), definition=function(x, decreasing=FALSE, ...) {
  types <- x@list %>% purrr::map_chr(~as.character(class(.x)))
  indexes1 <- x@list %>% purrr::map_int(~.x@index)
  indexes2 <- x@list %>% purrr::map_int(.f=function(.x){
    if("index2" %in% methods::slotNames(.x)) {
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

#' @rdname standardise
setMethod("standardise", signature=c("parameters"), definition=function(object, ...) {
  list <- object@list %>% purrr::map(.f=function(param) {
    return(param %>% standardise(parameters=object))
  })
  retValue <- Parameters()
  retValue@list <- list
  return(retValue)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write subset of parameters (theta, omega or sigma).
#' 
#' @param object subset of parameters
#' @param file filename
#' @param ... extra arguments, like defaultDf for empty parameters list
#' @return TRUE if success
#' @importFrom dplyr select_if
#' @importFrom utils write.csv
writeParameters <- function(object, file, ...) {
  df <- purrr::map_df(object@list, .f=as.data.frame, row.names=character(), optional=FALSE)
  
  # Get rid of column if all NA
  df <- df %>% dplyr::select_if(.predicate=~!(is.logical(.x) && all(is.na(.x))))

  if (nrow(df)==0) {
    df <- processExtraArg(args=list(...), name="defaultDf", mandatory=TRUE)
  }
  utils::write.csv(df, file=file, row.names=FALSE)
  return(TRUE)
}

#' Write variance-covariance matrix.
#' 
#' @param object matrix
#' @param file filename
#' @return TRUE if success
#' @importFrom utils write.csv
writeVarcov <- function(object, file) {
  utils::write.csv(object, file=file)
  return(TRUE)
}

#' @rdname write
setMethod("write", signature=c("parameters", "character"), definition=function(object, file, ...) {
  theta <- object %>% select("theta")
  omega <- object %>% select("omega")
  sigma <- object %>% select("sigma")
  varcov <- object@varcov
  
  theta %>% writeParameters(file=file.path(file, "theta.csv"),
                  defaultDf=data.frame(name=character(), index=integer(), value=numeric(), fix=logical()))
  omega %>% writeParameters(file=file.path(file, "omega.csv"),
                  defaultDf=data.frame(name=character(), index=integer(), index2=integer(), value=numeric(), fix=logical(), type=character()))
  sigma %>% writeParameters(file=file.path(file, "sigma.csv"),
                  defaultDf=data.frame(name=character(), index=integer(), index2=integer(), value=numeric(), fix=logical(), type=character()))
  
  if (length(varcov) > 0) {
    varcov %>% writeVarcov(file=file.path(file, "varcov.csv"))
  }
})

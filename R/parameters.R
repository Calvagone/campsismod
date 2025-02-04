
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
#' @importFrom purrr discard map_chr
#' @importFrom assertthat are_equal
#' @keywords internal
#' 
appendParameters <- function(params1, params2) {
  getParameterNamesInModel <- function(parameters) {
    retValue <- parameters@list %>%
      purrr::map_chr(.f=function(parameter) {
        if (is(parameter, "double_array_parameter") && !isDiag(parameter)) {
          return(NA)
        } else {
          return(parameter %>% getNameInModel())
        }
      }) %>%
      purrr::discard(~is.na(.x))
    return(retValue)
  }
  paramNames1 <- getParameterNamesInModel(params1)
  paramNames2 <- getParameterNamesInModel(params2)
  
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

  # Merge variance-covariance matrices
  varcov1 <- params1@varcov
  varcov2 <- params2@varcov
  params1@varcov <- appendVarcov(varcov1, varcov2)

  return(params1 %>% sort())
}

appendVarcov <- function(varcov1, varcov2) {
  if (length(varcov1) == 0 && length(varcov2) > 0) {
    return(varcov2)
  
  } else if (length(varcov2) == 0 && length(varcov1) > 0) {
    return(varcov1)
  
  } else if (length(varcov1) == 0 && length(varcov2) == 0) {
    return(matrix(numeric(0), nrow=0, ncol=0))
  
  } else {
    dimnames1 <- dimnames(varcov1)
    assertthat::are_equal(dimnames1[[1]], dimnames1[[2]])
    colnames1 <- dimnames1[[1]]
    
    dimnames2 <- dimnames(varcov2)
    assertthat::are_equal(dimnames2[[1]], dimnames2[[2]])
    colnames2 <- dimnames2[[1]]
    
    colnames <- c(colnames1, colnames2)
    assertthat::assert_that(!any(duplicated(colnames)), msg="Duplicate parameter names in resulting variance-covariance matrix")
    totalDim <- length(colnames)
    
    varcov <- matrix(numeric(length(totalDim)^2), nrow=totalDim, ncol=totalDim)
    dimnames(varcov) <- list(colnames, colnames)
    
    varcov[seq_along(colnames1), seq_along(colnames1)] <- varcov1
    varcov[length(colnames1) + seq_along(colnames2), length(colnames1) + seq_along(colnames2)] <- varcov2
    return(varcov)
  }
}

#_______________________________________________________________________________
#----                             addRSE                                    ----
#_______________________________________________________________________________

#' @rdname addRSE
setMethod("addRSE", signature=c("parameters", "parameter", "numeric"), definition=function(object, parameter, value, ...) {
  parameter_ <- object %>%
    find(parameter)
  
  if (is.null(parameter_)) {
    stop("Parameter ", parameter %>% getName(), " not found in model")
  }
  
  # Define variance-covariance matrix (single value)
  varcov <- matrix((value/100*abs(parameter_@value))^2, nrow=1, ncol=1)
  name <- parameter_ %>% getName()
  dimnames(varcov) <- list(name, name)
  
  # Remove last value if it exists
  colnames <- colnames(object@varcov)
  if (name %in% colnames) {
    object@varcov <- object@varcov[-which(name==colnames), -which(name==colnames), drop=FALSE]
  }
  
  # Update variance-covariance matrix
  object@varcov <- appendVarcov(object@varcov, varcov)
  
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
#' @importFrom purrr discard keep map
setMethod("disable", signature=c("parameters", "character"), definition=function(object, x, ...) {
  variabilities <- c("IIV", "IOV", "RUV", "VARCOV", "VARCOV_OMEGA", "VARCOV_SIGMA")
  msg <- paste0("Only these variabilities can be disabled: ", paste0("'", variabilities, "'", collapse=", "))
  assertthat::assert_that(list(...) %>% length()==0, msg="Extra arguments are not accepted")
  assertthat::assert_that(all(x %in% variabilities), msg=msg)
  
  # Disable IIV
  if ("IIV" %in% x) {
    omega_ <- (object %>% select("omega"))@list %>%
      purrr::map(.f=function(param) {
          param@value <- 0
          return(param)
        })
    object <- object %>% replace(omega_)
  }
  
  # Disable IOV (note that IOV is a subset of IIV)
  if ("IOV" %in% x) {
    omega_ <- (object %>% select("omega"))@list %>%
      purrr::discard(.p=~is.na(.x@same)) %>%
      purrr::map(.f=function(param) {
          param@value <- 0
          return(param)
        })
    object <- object %>% replace(omega_)
  }
  
  # Disable RUV
  if ("RUV" %in% x) {
    sigma_ <- (object%>% select("sigma"))@list %>%
      purrr::map(.f=function(param) {
        param@value <- 0
        return(param)
      })
    object <- object %>% replace(sigma_)
  }
  
  # Disable VARCOV (variance covariance matrix)
  if ("VARCOV" %in% x) {
    object@varcov <- matrix(numeric(0), ncol=0, nrow=0)
  }
  
  # Disable all omegas or sigmas in varcov
  varcovOmega <- "VARCOV_OMEGA" %in% x
  varcovSigma <- "VARCOV_SIGMA" %in% x
  
  if (varcovOmega || varcovSigma) {
    # Retrieve varcov parameters to remove
    varcovParams <- colnames(object@varcov) %>%
      purrr::map(.f=function(.x) {
        return(object %>% getByName(.x))
      }) %>%
      purrr::keep(.p=~(is(.x, "omega") && varcovOmega) ||
                    (is(.x, "sigma") && varcovSigma))
    
    # Retrieve the corresponding indexes in the matrix
    indexesToRemove <- varcovParams %>%
      purrr::map_int(.f=~which(colnames(object@varcov) == .x %>% getName()))
    
    # Update variance-covariance matrix
    if (length(indexesToRemove) > 0) {
      object@varcov <- object@varcov[-indexesToRemove, -indexesToRemove, drop=FALSE]
    }
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
  object <- object %>% sort()

  # We need at least to elements
  if (length(object@list) < 2) {
    return(object)
  }
  
  # Select omega's only
  omegas <- object %>% select("omega")
  
  # Checking all 'same' are NA's
  sameVector <- omegas@list %>% purrr::map_lgl(.f=~.x@same)
  assertthat::assert_that(all(is.na(sameVector)), msg="all 'same' must be NA")

  # New list of omega's, add first omega into the list 
  omegas_ <- Parameters()
  omegas_ <- omegas_ %>% add(omegas@list[[1]])
  
  # Fix NA problems
  # .x is the accumulated results or initial value (a 'parameters' object here)
  # .y next value in sequence (an omega here)
  returned_omega_ <- purrr::accumulate(.x=omegas@list[2:length(omegas@list)], .f=function(.x, .y) {
    lastOmega <- .x@list[[.x@list %>% length()]]
    currentOmega <- .y
    
    # Is my current omega SAME as previous?
    if (is.na(currentOmega@value)) {
      currentOmega@value <- lastOmega@value
      currentOmega@same <- TRUE
      if (is.na(lastOmega@same)) {
        lastOmega@same <- FALSE
        # Update first SAME omega
        .x <- .x %>% replace(lastOmega) 
      }
    }
    # Update slot 'fix' based on last omega
    if (is.na(currentOmega@fix)) {
      currentOmega@fix <- lastOmega@fix
    }
    
    # Accumulate here
    .x <- .x %>% add(currentOmega)
    
    return(.x)
  }, .init=omegas_)
  
  # Replace all previous omega's by new ones
  object <- object %>% replace(returned_omega_)

  return(object)
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
#----                          getUncertainty                               ----
#_______________________________________________________________________________

#' @importFrom tibble tibble
#' @rdname getUncertainty
setMethod("getUncertainty", signature=c("parameters"), definition=function(object, ...) {
  varcov <- object %>% getVarCov()
  if (is.null(varcov)) {
    return(tibble::tibble(name=character(0), se=numeric(0), "rse%"=numeric(0)))
  } else {
    return(object@list %>%
             purrr::map_df(.f=~getUncertainty(object=.x, varcov=varcov, parameters=object)))
  }
})

#_______________________________________________________________________________
#----                             getVarCov                                 ----
#_______________________________________________________________________________

#' @rdname getVarCov
setMethod("getVarCov", signature=c("parameters"), definition=function(object) {
  varcov <- object@varcov
  if (varcov %>% length() == 0) {
    return(NULL)
  } else {
    return(varcov)
  }
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
  name <- ifelse(is.null(row$name), NA, row$name) # Optional
  label <- ifelse(is.null(row$label), as.character(NA), row$label) # Optional
  unit <- ifelse(is.null(row$unit), as.character(NA), row$unit) # Optional
  comment <- ifelse(is.null(row$comment), as.character(NA), row$comment) # Optional
  
  if (type=="theta") {
    param <- Theta(name=name, index=row$index, value=row$value, fix=row$fix, label=label, unit=unit, comment=comment)
  } else if(type=="omega") {
    same <- ifelse(is.null(row$same), NA, row$same) # Optional
    param <- Omega(name=name, index=row$index, index2=row$index2, value=row$value, fix=row$fix, type=row$type, same=same, label=label, comment=comment)
  } else if(type=="sigma") {
    param <- Sigma(name=name, index=row$index, index2=row$index2, value=row$value, fix=row$fix, type=row$type, label=label, comment=comment)
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
#' @importFrom dplyr across group_split
#' @importFrom purrr map
#' @export
read.parameters <- function(file, type) {
  assertthat::assert_that(type %in% c("theta", "omega", "sigma"),
                          msg="Type must be one of these: 'theta', 'omega' or 'sigma'")
  df <- readr::read_delim(file=file, lazy=FALSE, show_col_types=FALSE, progress=FALSE) %>%
    dplyr::mutate(ROWID=dplyr::row_number())
  list <- df %>%
    dplyr::group_split(dplyr::across("ROWID")) %>%
    purrr::map(~dataframeToParameter(as.list(.x), type=type))
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

  parameters <-  Parameters() %>%
    add(theta) %>%
    add(omega) %>%
    add(sigma)

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
#----                            setMinMax                                  ----
#_______________________________________________________________________________

#' @rdname setMinMax
setMethod("setMinMax", signature=c("parameters", "parameter", "numeric", "numeric"), definition=function(object, parameter, min, max, ...) {
  parameter_ <- object %>%
    find(parameter)
  
  if (is.null(parameter_)) {
    stop("Parameter ", parameter %>% getNameInModel(), " not found in model")
  }
  
  # Replace old values
  parameter_@min <- min
  parameter_@max <- max
  
  # Replace old parameter
  object <- object %>%
    replace(parameter_)
  
  return(object)
})

#' @rdname setMinMax
setMethod("setMinMax", signature=c("parameters", "character", "numeric", "numeric"), definition=function(object, parameter, min, max, ...) {
  assertthat::assert_that(parameter %in% c("theta", "omega", "sigma"), msg="Parameter must be one of: 'theta', 'omega' or 'sigma'")
  object@list <- object@list %>%
    purrr::map(.f=function(x) {
      if (is(x, parameter)) {
        x@min <- min
        x@max <- max
        
        # Special case for covariance
        if (is(x, "double_array_parameter") && !(x %>% isDiag()) && min >= 0) {
          x@min <- -max
          cat(sprintf("Info: min value of %s (covariance) set to -max value\n", x %>% getName()))
        }
      }
      return(x)
    })
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

showUncertaintyOnParameters <- function(parameters, discard_na_columns=NULL) {
  retValue <- purrr::map_df(parameters@list, .f=as.data.frame, row.names=character(), optional=FALSE) %>%
    removeNaColumn(discard_na_columns)
  
  if (parameters %>% length() > 0) {
    uncertainty <- parameters %>% getUncertainty()
    # Show uncertainty if at least one parameter has uncertainty
    if (any(!is.na(uncertainty$se))) {
      retValue <- dplyr::bind_cols(retValue, uncertainty %>% dplyr::select(-"name")) 
    }
  }
  return(retValue)
}

setMethod("show", signature=c("parameters"), definition=function(object) {
  cat("THETA's:\n")
  print(showUncertaintyOnParameters(object %>% select("theta"), discard_na_columns=c("min", "max", "label", "unit", "comment")))
  cat("OMEGA's:\n")
  print(showUncertaintyOnParameters(object %>% select("omega"), discard_na_columns=c("min", "max", "same", "label", "comment")))
  cat("SIGMA's:\n")
  print(showUncertaintyOnParameters(object %>% select("sigma"), discard_na_columns=c("min", "max", "label", "comment")))
  if (is.null(object %>% getVarCov())) {
    cat("No variance-covariance matrix\n")
  } else {
    cat("Variance-covariance matrix available (see ?getVarCov)\n")
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
  retValue@varcov <- object@varcov
  return(retValue)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write subset of parameters (theta, omega or sigma).
#' 
#' @param object subset of parameters
#' @param file filename
#' @param ... extra arguments, like \code{defaultDf} for empty parameters list
#' @return TRUE if success
#' @importFrom dplyr any_of select where 
#' @importFrom utils write.csv
writeParameters <- function(object, file, ...) {
  df <- purrr::map_df(object@list, .f=as.data.frame, row.names=character(), optional=FALSE)
  
  # Get rid of specific columns if all NA
  naColumns <- c("min", "max", "fix", "same", "label", "unit", "comment")
  df <- df %>% removeNaColumn(naColumns)
  
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

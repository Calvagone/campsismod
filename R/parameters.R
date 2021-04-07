
#_______________________________________________________________________________
#----                          parameters class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "parameters",
  representation(
    varcov = "matrix"
  ),
  contains = "pmx_list",
  prototype = prototype(type="parameter", varcov=matrix(numeric(0), nrow=0, ncol=0))
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
  msg <- "Only these 3 variabilities can be disabled for now: 'IIV', 'RUV', 'VARCOV'"
  variabilities <- c("IIV", "RUV", "VARCOV")
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
  # Disable VARCOV (variance covariance matrix)
  if ("VARCOV" %in% x) {
    object@varcov <- matrix(numeric(0), ncol=0, nrow=0)
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
#' @export
read.parameters <- function(file, type) {
  assertthat::assert_that(type %in% c("theta", "omega", "sigma"),
                          msg="Type must be one of these: 'theta', 'omega' or 'sigma'")
  df <- read.csv(file=file) %>% dplyr::mutate(ROWID=dplyr::row_number())
  list <- df %>% plyr::dlply(.variables="ROWID", .fun=dataframeToParameter, type=type)
  attributes(list) <- NULL
  return(new("parameters", list=list))
}

#' Read variance-covariance file.
#' 
#' @param file path to CSV file
#' @return variance-covariance matrix
#' @importFrom assertthat assert_that
#' @export
read.varcov <- function(file) {
  dataframe <- read.csv(file=file)
  row.names(dataframe) <- dataframe$X
  matrix <- dataframe %>% select(-X) %>% as.matrix()
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
  
  if (!file.exists(thetaPath)) {
    stop(paste0("Theta file couln't be found."))
  }
  if (!file.exists(omegaPath)) {
    stop(paste0("Omega file couln't be found."))
  }
  if (!file.exists(sigmaPath)) {
    stop(paste0("Sigma file couln't be found."))
  }

  theta <- read.parameters(file=thetaPath, type="theta")
  omega <- read.parameters(file=omegaPath, type="omega")
  sigma <- read.parameters(file=sigmaPath, type="sigma")
  
  parameters <- new("parameters", list=c(theta@list, omega@list, sigma@list)) %>% clean()
  if (file.exists(varcovPath)) {
    varcov <- read.varcov(varcovPath)
    parameters@varcov <- varcov
  }
  return(parameters)
}

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

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write subset of parameters (theta, omega or sigma).
#' 
#' @param object subset of parameters
#' @param file filename
#' @return TRUE if success
#' @importFrom dplyr select_if
writeParameters <- function(object, file, ...) {
  df <- purrr::map_df(object@list, .f=as.data.frame, row.names=character(), optional=FALSE)
  
  # Get rid of column if all NA
  df <- df %>% dplyr::select_if(.predicate=~!(is.logical(.x) && all(is.na(.x))))

  if (nrow(df)==0) {
    df <- processExtraArg(args=list(...), name="defaultDf", mandatory=TRUE)
  }
  write.csv(df, file=file, row.names=FALSE)
  return(TRUE)
}

#' Write variance-covariance matrix.
#' 
#' @param object matrix
#' @param file filename
#' @return TRUE if success
writeVarcov <- function(object, file, ...) {
  write.csv(object, file=file)
  return(TRUE)
}

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

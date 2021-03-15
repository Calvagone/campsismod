
#' PMX model class.
#' 
#' @export
setClass(
  "pmx_model",
  representation(
    model = "code_records",      # Mandatory
    parameters = "parameters" # Mandatory    
  )
)

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("pmx_model", "character"), definition=function(object, file, zip=TRUE) {
  model <- object@model
  parameters <- object@parameters
  theta <- parameters %>% select("theta")
  omega <- parameters %>% select("omega")
  sigma <- parameters %>% select("sigma")
  
  if (zip) {
    
  } else {
    if (dir.exists(file)) {
      # do nothing
    } else {
      dir.create(file)
    }
    model %>% write(file=file.path(file, "model.mod"))
    theta %>% write(file=file.path(file, "theta.csv"),
                    defaultDf=data.frame(name=character(), index=integer(), value=numeric(), fix=logical()))
    omega %>% write(file=file.path(file, "omega.csv"),
                    defaultDf=data.frame(name=character(), index=integer(), index2=integer(), value=numeric(), fix=logical(), type=character()))
    sigma %>% write(file=file.path(file, "sigma.csv"),
                    defaultDf=data.frame(name=character(), index=integer(), index2=integer(), value=numeric(), fix=logical(), type=character()))
  }
})

setMethod("write", signature=c("parameters", "character"), definition=function(object, file, ...) {
  df <- purrr::map_df(object@list, .f=as.data.frame, row.names=character(), optional=FALSE)
  if (nrow(df)==0) {
    df <- list(...)$defaultDf
  }
  write.csv(df, file=file, row.names=FALSE)
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

dataframeToParameter <- function(row, type) {
  param <- NULL
  
  if (type=="theta") {
    param <- new("theta", name=as.character(row$name), index=row$index, value=row$value, fix=row$fix)
  } else if(type=="omega" | type=="sigma") {
    param <- new(type, name=as.character(row$name), index=row$index, index2=row$index2, value=row$value, fix=row$fix, type=row$type)
  } else {
    stop(paste0("type must be one of: theta, omega or sigma"))
  }
  return(param)
}

#' Read PMX model file.
#' 
#' @param file path to folder or path to zipped project
#' @return a PMX model
#' @export
read.pmxmod <- function(file) {
  folder <- NULL
  if (dir.exists(file)) {
    folder <- file
  } else if (file.exists(file)) {
    
  } else {
    stop("file is not a ZIP file nor a valid folder")
  }
  
  modelPath <- file.path(folder, "model.mod")
  thetaPath <- file.path(folder, "theta.csv")
  omegaPath <- file.path(folder, "omega.csv")
  sigmaPath <- file.path(folder, "sigma.csv")
  
  if (!file.exists(modelPath)) {
    stop(paste0("Model file couln't be found."))
  }
  if (!file.exists(thetaPath)) {
    stop(paste0("Theta file couln't be found."))
  }
  if (!file.exists(omegaPath)) {
    stop(paste0("Omega file couln't be found."))
  }
  if (!file.exists(sigmaPath)) {
    stop(paste0("Sigma file couln't be found."))
  }
  
  model <- read.model(file=modelPath)
  theta <- read.parameter(file=thetaPath, type="theta")
  omega <- read.parameter(file=omegaPath, type="omega")
  sigma <- read.parameter(file=sigmaPath, type="sigma")
  
  list <- c(theta@list, omega@list, sigma@list)
  
  return(new("pmx_model", model=model, parameters=new("parameters", list=list) %>% clean()))
}

#' Read parameter file.
#' 
#' @param file path to folder or path to zipped project
#' @param type parameter type: 'theta', 'omega' or 'sigma'
#' @return a PMX model
#' @export
read.parameter <- function(file, type) {
  assertthat::assert_that(type %in% c("theta", "omega", "sigma"),
                          msg="Type must be one of these: 'theta', 'omega' or 'sigma'")
  df <- read.csv(file=file) %>% dplyr::mutate(ROWID=dplyr::row_number())
  list <- df %>% plyr::dlply(.variables="ROWID", .fun=dataframeToParameter, type=type)
  attributes(list) <- NULL
  return(new("parameters", list=list))
}

#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' RxODE export type class.
#' 
#' @export
setClass(
  "rxode_type",
  representation(
  ),
  contains="export_type" 
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________


setMethod("export", signature=c("pmx_model", "character"), definition=function(object, dest) {
  if (dest=="RxODE") {
    return(object %>% export(new("rxode_type")))
  } else {
    stop("Only RxODE is supported for now")
  }
})
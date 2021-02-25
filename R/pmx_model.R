
setClass(
  "pmx_model",
  representation(
    model = "code_records",      # Mandatory
    parameters = "parameters" # Mandatory    
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Export function.
#' 
#' @param object generic object
#' @param dest destination
#' @return specific object depending on given destination
#' @export
export <- function(object, dest) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest) {
  standardGeneric("export")
})

setMethod("export", signature=c("pmx_model", "character"), definition=function(object, dest) {
  if (dest=="RxODE") {
    return(
      new(
        "rxode_model",
        code = object@code,
        theta = rxodeParams(object),
        omega = rxodeMatrix(object, type = "omega"),
        sigma = rxodeMatrix(object, type = "sigma")
      )
    )
  } else {
    stop("Only RxODE is supported for now")
  }
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("pmx_model", "character"), definition=function(object, file, zip=TRUE) {
  model <- object@model
  parameters <- object@parameters
  theta <- parameters %>% filter(type="theta")
  omega <- parameters %>% filter(type="omega")
  sigma <- parameters %>% filter(type="sigma")
  
  if (zip) {
    
  } else {
    if (dir.exists(file)) {
      # do nothing
    } else {
      dir.create(file)
    }
    model %>% write(file=file.path(file, "model.mod"))
    theta %>% write(file=file.path(file, "theta.csv"))
    omega %>% write(file=file.path(file, "omega.csv"))
    sigma %>% write(file=file.path(file, "sigma.csv"))
  }
})

s4ToDataframe <- function(object) {
  names <- slotNames(object)
  list <- lapply(names, function(name) slot(object, name))
  as.data.frame(setNames(list, names))
}

parametersToDataframe <- function(object) {
  retValue <- purrr::map_df(object@list, .f=s4ToDataframe)
  return(retValue)
}

setMethod("write", signature=c("parameters", "character"), definition=function(object, file) {
  df <- parametersToDataframe(object)
  
  # Order columns in CSV files
  cols <- c("name", "index", "index2", "value", "fix")
  colnames <- factor(colnames(df), levels=cols, labels=cols)
  df <- df[, base::order(colnames)]
  
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
    param <- new(type, name=as.character(row$name), index=row$index, index2=row$index2, value=row$value, fix=row$fix)
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

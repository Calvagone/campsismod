
#' PMX model class.
#' 
#' @export
setClass(
  "pmx_model",
  representation(
    model = "code_records",
    parameters = "parameters",
    compartments = "compartments"
  )
)

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature=c("pmx_model", "compartment_characteristic"), definition=function(object, x) {
  compartment <- object@compartments %>% getByIndex(Compartment(index=x@compartment))
  if (length(compartment) == 0) {
    stop(paste0("Unable to find compartment ", x@compartment, " in PMX model"))
  }
  
  # Add characteristic (delegate to add method in compartments class)
  object@compartments <- object@compartments %>% add(x) 
  
  return(object)
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

setMethod("disable", signature=c("pmx_model", "character"), definition=function(object, x, ...) {
  object@parameters <- object@parameters %>% disable(x=x, ...)
  return(object)
})

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

#' Mrgsolve export type class.
#' 
#' @export
setClass(
  "mrgsolve_type",
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
  } else if (dest=="mrgsolve") {
    return(object %>% export(new("mrgsolve_type")))
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
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
  
  records <- read.model(file=modelPath)
  theta <- read.parameter(file=thetaPath, type="theta")
  omega <- read.parameter(file=omegaPath, type="omega")
  sigma <- read.parameter(file=sigmaPath, type="sigma")
  
  model <- new("pmx_model", model=records,
      parameters=new("parameters", list=c(theta@list, omega@list, sigma@list)) %>% clean(),
      compartments=Compartments())
  return(model %>% updateCompartments())
}

#' Update compartments list from the persisted records.
#' Exported especially for package pmxtran. However, this method should not be called.
#' 
#' @param model PMX model
#' @return an updated PMX model, with an updated compartments list
#' @export
updateCompartments <- function(model) {
  if (!is(model, "pmx_model")) {
    stop("model is not a PMX model")   
  }
  records <- model@model
  returnedList <- records %>% getCompartments()
  
  updatedDesRecord <- returnedList[[1]]
  compartments <- returnedList[[2]]
  if (length(updatedDesRecord) > 0) {
    records <- records %>% replace(updatedDesRecord)
  }
  model@model <- records
  model@compartments <- compartments
  return(model)
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
#----                                 write                                 ----
#_______________________________________________________________________________

setMethod("write", signature=c("pmx_model", "character"), definition=function(object, file, ...) {
  zip <- processExtraArg(args=list(...), name="zip", default=FALSE)
  records <- object@model
  parameters <- object@parameters

  if (zip) {
    
  } else {
    if (dir.exists(file)) {
      # do nothing
    } else {
      dir.create(file)
    }
    records %>% write(file=file.path(file, "model.mod"), model=object)
    parameters %>% write(file=file)
  }
  return(TRUE)
})

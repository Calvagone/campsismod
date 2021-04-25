
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

setMethod("add", signature=c("pmx_model", "compartment_initial_condition"), definition=function(object, x) {
  compartment <- object@compartments %>% getByIndex(Compartment(index=x@compartment))
  if (length(compartment) == 0) {
    stop(paste0("Unable to find compartment ", x@compartment, " in PMX model"))
  }
  
  # Add initial condition (delegate to add method in compartments class)
  object@compartments <- object@compartments %>% add(x) 
  
  return(object)
})

setMethod("add", signature=c("pmx_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% add(x)
  return(object)
})

setMethod("add", signature=c("pmx_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% add(x)
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
#----                          getCompartmentIndex                          ----
#_______________________________________________________________________________

setMethod("getCompartmentIndex", signature=c("pmx_model", "character"), definition=function(object, name) {
  return(object@compartments %>% getCompartmentIndex(name=name))
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

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
  
  modelPath <- file.path(folder, "model.pmx")
  
  if (!file.exists(modelPath)) {
    stop(paste0("Model file couln't be found."))
  }
  
  records <- read.model(file=modelPath)
  parameters <- read.allparameters(folder=folder)
  
  model <- new("pmx_model", model=records, parameters=parameters, compartments=Compartments())
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

#_______________________________________________________________________________
#----                           removeEquation                              ----
#_______________________________________________________________________________

setMethod("removeEquation", signature=c("pmx_model"), definition=function(object, lhs) {
  object@model <- object@model %>% removeEquation(lhs)
  return(object)
})

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

setMethod("replace", signature=c("pmx_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% replace(x)
  return(object)
})

setMethod("replace", signature=c("pmx_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% replace(x)
  return(object)
})

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
    records %>% write(file=file.path(file, "model.pmx"), model=object)
    parameters %>% write(file=file)
  }
  return(TRUE)
})

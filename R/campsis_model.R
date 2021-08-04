
#' CAMPSIS model class.
#' 
#' @export
setClass(
  "campsis_model",
  representation(
    model = "code_records",
    parameters = "parameters",
    compartments = "compartments"
  )
)

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature=c("campsis_model", "compartment_property"), definition=function(object, x) {
  compartment <- object@compartments %>% getByIndex(Compartment(index=x@compartment))
  if (length(compartment) == 0) {
    stop(paste0("Unable to find compartment ", x@compartment, " in PMX model"))
  }
  
  # Add characteristic (delegate to add method in compartments class)
  object@compartments <- object@compartments %>% add(x) 
  
  return(object)
})

#' @rdname add
setMethod("add", signature=c("campsis_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature=c("campsis_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature=c("campsis_model", "campsis_model"), definition=function(object, x) {
  object <- object %>% appendModel(x)
  return(object)
})

#' Append model (or simply add).
#' 
#' @param model1 base model
#' @param model2 model to append
#' @return the resulting CAMPSIS model
appendModel <- function(model1, model2) {
  # Append compartments (and included properties)
  model1@parameters <- model1@parameters %>% add(model2@parameters)
  
  # Append code records
  model1@model <- model1@model %>% add(model2@model)
  
  # Append compartments (and included properties)
  model1@compartments <- model1@compartments %>% add(model2@compartments)
  return(model1)
}

#_______________________________________________________________________________
#----                            addEquation                                ----
#_______________________________________________________________________________

#' @rdname addEquation
setMethod("addEquation", signature=c("campsis_model", "character", "character"), definition=function(object, lhs, rhs, before=NULL, after=NULL) {
  object@model <- object@model %>% addEquation(lhs=lhs, rhs=rhs, before=before, after=after)
  return(object)
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' @rdname disable
setMethod("disable", signature=c("campsis_model", "character"), definition=function(object, x, ...) {
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

#' @rdname export
setMethod("export", signature=c("campsis_model", "character"), definition=function(object, dest, outvars=NULL) {
  if (dest=="RxODE") {
    return(object %>% export(new("rxode_type")))
  } else if (dest=="mrgsolve") {
    return(object %>% export(new("mrgsolve_type"), outvars=outvars))
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
})

#_______________________________________________________________________________
#----                          getCompartmentIndex                          ----
#_______________________________________________________________________________

#' @rdname getCompartmentIndex
setMethod("getCompartmentIndex", signature=c("campsis_model", "character"), definition=function(object, name) {
  return(object@compartments %>% getCompartmentIndex(name=name))
})

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

#' @rdname getEquation
setMethod("getEquation", signature=c("campsis_model", "character"), definition=function(object, lhs) {
  return(object@model %>% getEquation(lhs))
})

#_______________________________________________________________________________
#----                            hasEquation                                ----
#_______________________________________________________________________________

#' @rdname hasEquation
setMethod("hasEquation", signature=c("campsis_model", "character"), definition=function(object, lhs) {
  return(object@model %>% hasEquation(lhs))
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Read a CAMPSIS model.
#' 
#' @param file path to folder
#' @return a CAMPSIS model
#' @export
read.campsis <- function(file) {
  folder <- NULL
  if (dir.exists(file)) {
    folder <- file
  } else if (file.exists(file)) {
    
  } else {
    stop("file is not a ZIP file nor a valid folder")
  }
  
  # model.campsis and model.pmx are both accepted
  modelPath <- file.path(folder, "model.campsis")
  if (!file.exists(modelPath)) {
    modelPath <- file.path(folder, "model.pmx")
    warning("Please rename your model file to 'model.campsis'")
    if (!file.exists(modelPath)) {
      stop(paste0("Model file couln't be found."))
    }
  }
  
  records <- read.model(file=modelPath)
  parameters <- read.allparameters(folder=folder)
  
  model <- new("campsis_model", model=records, parameters=parameters, compartments=Compartments())
  return(model %>% updateCompartments())
}


#' Read a CAMPSIS model (deprecated).
#' 
#' @param file path to folder
#' @return a CAMPSIS model
#' @export
read.pmxmod <- function(file) {
  .Deprecated("read.campsis")
  return(read.campsis(file=file))
}

#' Update compartments list from the persisted records.
#' Exported especially for package pmxtran. However, this method should not be called.
#' 
#' @param model PMX model
#' @return an updated PMX model, with an updated compartments list
#' @export
updateCompartments <- function(model) {
  if (!is(model, "campsis_model")) {
    stop("model is not a CAMPSIS model")   
  }
  records <- model@model
  
  # Get list of compartments
  compartments <- records %>% getCompartments()
  
  # Extract characteristics
  compartments <- compartments %>% addProperties(records, "F", init=Bioavailability(0, rhs=""))
  compartments <- compartments %>% addProperties(records, "LAG", init=LagTime(0, rhs=""))
  compartments <- compartments %>% addProperties(records, "DURATION", init=InfusionDuration(0, rhs=""))
  compartments <- compartments %>% addProperties(records, "RATE", init=InfusionRate(0, rhs=""))
  compartments <- compartments %>% addProperties(records, "INIT", init=InitialCondition(0, rhs=""))
  
  # Remove transient records because information is now found in properties
  records <- records %>% removeTransientRecords()

  model@model <- records
  model@compartments <- compartments
  return(model)
}

#_______________________________________________________________________________
#----                           removeEquation                              ----
#_______________________________________________________________________________

#' @rdname removeEquation
setMethod("removeEquation", signature=c("campsis_model", "character"), definition=function(object, lhs) {
  object@model <- object@model %>% removeEquation(lhs)
  return(object)
})

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature=c("campsis_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature=c("campsis_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

#' @rdname replaceEquation
setMethod("replaceEquation", signature=c("campsis_model", "character", "character"), definition=function(object, lhs, rhs) {
  object@model <- object@model %>% replaceEquation(lhs, rhs)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("campsis_model"), definition=function(object) {
  show(object@model %>% addTransientRecords(model=object))
  cat("\n")
  show(object@parameters)
  cat("\n")
  show(object@compartments)
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

#' @rdname sort
setMethod("sort", signature=c("campsis_model"), definition=function(x, decreasing=FALSE, ...) {
  # Sort code records
  x@model <- x@model %>% sort()
  
  # Sort compartments (properties will be sorted correctly)
  x@compartments <- x@compartments %>% sort()
  
  # Sort parameters
  x@parameters <- x@parameters %>% sort()
  
  return(x)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' @rdname write
setMethod("write", signature=c("campsis_model", "character"), definition=function(object, file, ...) {
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
    records %>% write(file=file.path(file, "model.campsis"), model=object)
    parameters %>% write(file=file)
  }
  return(TRUE)
})


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

#' @rdname add
setMethod("add", signature=c("pmx_model", "compartment_property"), definition=function(object, x) {
  compartment <- object@compartments %>% getByIndex(Compartment(index=x@compartment))
  if (length(compartment) == 0) {
    stop(paste0("Unable to find compartment ", x@compartment, " in PMX model"))
  }
  
  # Add characteristic (delegate to add method in compartments class)
  object@compartments <- object@compartments %>% add(x) 
  
  return(object)
})

#' @rdname add
setMethod("add", signature=c("pmx_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature=c("pmx_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% add(x)
  return(object)
})

#' @rdname add
setMethod("add", signature=c("pmx_model", "pmx_model"), definition=function(object, x) {
  object <- object %>% appendModel(x)
  return(object)
})

#' Append model (or simply add).
#' 
#' @param model1 base model
#' @param model2 model to append
#' @return the resulting CAMPSIS model
appendModel <- function(model1, model2) {
  paramNames1 <- model1@parameters %>% getNames()
  paramNames2 <- model2@parameters %>% getNames()
  cmtNames1 <- model1@compartments %>% getNames()
  cmtNames2 <- model2@compartments %>% getNames()
  
  checkCollisionOnParams <- paramNames1 %in% paramNames2
  if (any(checkCollisionOnParams)) {
    stop(paste0("Model can't be appended because of duplicate parameter name(s): ", paste0(paramNames1[checkCollisionOnParams], collapse=", ")))
  }
  checkCollisionOnCmts <- cmtNames1 %in% cmtNames2
  if (any(checkCollisionOnCmts)) {
    stop(paste0("Model can't be appended because of duplicate compartment name(s): ", paste0(cmtNames1[checkCollisionOnCmts], collapse=", ")))
  }
  
  thetaMax <- model1@parameters %>% select("theta") %>% maxIndex()
  omegaMax <- model1@parameters %>% select("omega") %>% maxIndex()
  sigmaMax <- model1@parameters %>% select("sigma") %>% maxIndex()
  cmtMax <- model1@compartments %>% length()
  
  for (theta in (model2@parameters %>% select("theta"))@list) {
    theta@index <- theta@index + thetaMax
    model1 <- model1 %>% add(theta)
  }
  for (omega in (model2@parameters %>% select("omega"))@list) {
    omega@index <- omega@index + omegaMax
    omega@index2 <- omega@index2 + omegaMax
    model1 <- model1 %>% add(omega)
  }
  for (sigma in (model2@parameters %>% select("sigma"))@list) {
    sigma@index <- sigma@index + sigmaMax
    sigma@index2 <- sigma@index2 + sigmaMax
    model1 <- model1 %>% add(sigma)
  }
  for (record in (model2@model)@list) {
    baseRecord <- model1@model %>% getByName(record %>% getName())
    if (baseRecord %>% length() == 0) {
      model1 <- model1 %>% add(record)
    } else {
      baseRecord@code <- baseRecord@code %>% append(record@code)
      model1 <- model1 %>% replace(baseRecord)
    }
  }
  for (compartment in model2@compartments@list) {
    compartment@index <- compartment@index + cmtMax
    model1@compartments@list <- model1@compartments@list %>% append(compartment)
  }
  for (property in model2@compartments@properties@list) {
    property@compartment <- property@compartment + cmtMax
    model1@compartments <- model1@compartments %>% add(property)
  }
  
  # Sort function will sort all code records in the proper order
  return(model1 %>% sort())
}

#_______________________________________________________________________________
#----                            addEquation                                ----
#_______________________________________________________________________________

#' @rdname addEquation
setMethod("addEquation", signature=c("pmx_model", "character", "character"), definition=function(object, lhs, rhs, before=NULL, after=NULL) {
  object@model <- object@model %>% addEquation(lhs=lhs, rhs=rhs, before=before, after=after)
  return(object)
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' @rdname disable
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

#' @rdname export
setMethod("export", signature=c("pmx_model", "character"), definition=function(object, dest, outvars=NULL) {
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
setMethod("getCompartmentIndex", signature=c("pmx_model", "character"), definition=function(object, name) {
  return(object@compartments %>% getCompartmentIndex(name=name))
})

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

#' @rdname getEquation
setMethod("getEquation", signature=c("pmx_model", "character"), definition=function(object, lhs) {
  return(object@model %>% getEquation(lhs))
})

#_______________________________________________________________________________
#----                            hasEquation                                ----
#_______________________________________________________________________________

#' @rdname hasEquation
setMethod("hasEquation", signature=c("pmx_model", "character"), definition=function(object, lhs) {
  return(object@model %>% hasEquation(lhs))
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
setMethod("removeEquation", signature=c("pmx_model", "character"), definition=function(object, lhs) {
  object@model <- object@model %>% removeEquation(lhs)
  return(object)
})

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature=c("pmx_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature=c("pmx_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

#' @rdname replaceEquation
setMethod("replaceEquation", signature=c("pmx_model", "character", "character"), definition=function(object, lhs, rhs) {
  object@model <- object@model %>% replaceEquation(lhs, rhs)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("pmx_model"), definition=function(object) {
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
setMethod("sort", signature=c("pmx_model"), definition=function(x, decreasing=FALSE, ...) {
  # Sort code records
  x@model <- x@model %>% sort()
  
  # Sort compartment properties
  x@compartments@properties <- x@compartments@properties %>% sort()
  
  # Sort parameters
  x@parameters <- x@parameters %>% sort()
  
  return(x)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' @rdname write
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


#_______________________________________________________________________________
#----                         campsis_model class                           ----
#_______________________________________________________________________________

#' CAMPSIS model class.
#' 
#' @slot model a list of code records
#' @slot parameters model parameters
#' @slot compartments model compartments
#' @export
setClass(
  "campsis_model",
  representation(
    model = "code_records",
    parameters = "parameters",
    compartments = "compartments"
  )
)

#' Create a new CAMPSIS model.
#' 
#' @return a CAMPSIS model, empty
#' @export
CampsisModel <- function() {
  return(new("campsis_model"))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature=c("campsis_model", "compartment_property"), definition=function(object, x) {
  compartment <- object@compartments %>% find(Compartment(index=x@compartment))
  if (is.null(compartment)) {
    stop(paste0("Unable to find compartment ", x@compartment, " in CAMPSIS model"))
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

#' @param pos position where x needs to be added in list
#' @rdname add
setMethod("add", signature=c("campsis_model", "model_statement"), definition=function(object, x, pos=NULL) {
  if (is(x, "ode")) {
    object@compartments <- object@compartments %>% addODECompartment(ode=x)
  }
  object@model <- object@model %>% add(x, pos=pos)
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
#' @keywords internal
#' 
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
#----                         autoDetectNONMEM                              ----
#_______________________________________________________________________________

#' @rdname autoDetectNONMEM
setMethod("autoDetectNONMEM", signature=c("campsis_model"), definition=function(object, ...) {
  main <- object@model %>% getByName("MAIN")
  numberOfCmts <- object@compartments %>% length()
  
  for (cmtIndex in seq_len(numberOfCmts)) {
    # Search for bioavailability
    fVar <- paste0("F", cmtIndex)
    if (main %>% find(Equation(fVar)) %>% length() > 0) {
      object <- object %>% add(Bioavailability(cmtIndex, rhs=fVar))
    }
    # Search for infusion duration
    dVar <- paste0("D", cmtIndex)
    if (main %>% find(Equation(dVar)) %>% length() > 0) {
      object <- object %>% add(InfusionDuration(cmtIndex, rhs=dVar))
    }
    # Search for infusion rate
    rVar <- paste0("R", cmtIndex)
    if (main %>% find(Equation(rVar)) %>% length() > 0) {
      object <- object %>% add(InfusionRate(cmtIndex, rhs=rVar))
    }
    # Search for lag time
    alagVar <- paste0("ALAG", cmtIndex)
    if (main %>% find(Equation(alagVar)) %>% length() > 0) {
      object <- object %>% add(LagTime(cmtIndex, rhs=alagVar))
    }
  }
  
  return(object)
})

#_______________________________________________________________________________
#----                               contains                                ----
#_______________________________________________________________________________

#' @rdname contains
setMethod("contains", signature=c("campsis_model", "pmx_element"), definition=function(object, x) {
  return(!is.null(object %>% find(x)))
})

#_______________________________________________________________________________
#----                               delete                                  ----
#_______________________________________________________________________________

#' @rdname delete
setMethod("delete", signature=c("campsis_model", "compartment_property"), definition=function(object, x) {
  object@compartments <- object@compartments %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature=c("campsis_model", "parameter"), definition=function(object, x) {
  object@parameters <- object@parameters %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature=c("campsis_model", "code_record"), definition=function(object, x) {
  object@model <- object@model %>% delete(x)
  return(object)
})

#' @rdname delete
setMethod("delete", signature=c("campsis_model", "model_statement"), definition=function(object, x) {
  object@model <- object@model %>% delete(x)
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
#----                                find                                   ----
#_______________________________________________________________________________

#' @rdname find
setMethod("find", signature=c("campsis_model", "compartment"), definition=function(object, x) {
  return(object@compartments %>% find(x))
})

#' @rdname find
setMethod("find", signature=c("campsis_model", "compartment_property"), definition=function(object, x) {
  return(object@compartments %>% find(x))
})

#' @rdname find
setMethod("find", signature=c("campsis_model", "parameter"), definition=function(object, x) {
  return(object@parameters %>% find(x))
})

#' @rdname find
setMethod("find", signature=c("campsis_model", "code_record"), definition=function(object, x) {
  return(object@model %>% find(x))
})

#' @rdname find
setMethod("find", signature=c("campsis_model", "model_statement"), definition=function(object, x) {
  return(object@model %>% find(x))
})

#_______________________________________________________________________________
#----                          getCompartmentIndex                          ----
#_______________________________________________________________________________

#' @rdname getCompartmentIndex
setMethod("getCompartmentIndex", signature=c("campsis_model", "character"), definition=function(object, name) {
  return(object@compartments %>% getCompartmentIndex(name=name))
})

#_______________________________________________________________________________
#----                          getUncertainty                               ----
#_______________________________________________________________________________

#' @rdname getUncertainty
setMethod("getUncertainty", signature=c("campsis_model"), definition=function(object, ...) {
  return(object@parameters %>% getUncertainty(...))
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
  
  # Construct CAMPSIS model
  records <- read.model(file=modelPath)
  parameters <- read.allparameters(folder=folder)
  model <- new("campsis_model", model=records, parameters=parameters, compartments=Compartments())
  model <- model %>% updateCompartments()
  
  # Validate the whole model
  methods::validObject(model, complete=TRUE)
  
  return(model)
}


#' Read a CAMPSIS model (deprecated).
#' 
#' @param file path to folder
#' @return a CAMPSIS model
#' @export
#' @keywords internal
read.pmxmod <- function(file) {
  .Deprecated("read.campsis")
  return(read.campsis(file=file))
}

#' Update compartments list from the persisted records.
#' Exported especially for package pmxtran. However, this method should not be called.
#' 
#' @param model CAMPSIS model
#' @return an updated CAMPSIS model, with an updated compartments list
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
  
  # Remove properties records because information is found in properties
  records@list <- records@list %>% purrr::keep(~!is(.x, "properties_record"))

  model@model <- records
  model@compartments <- compartments
  return(model)
}

#_______________________________________________________________________________
#----                               replace                                 ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature=c("campsis_model", "compartment"), definition=function(object, x) {
  object@compartments <- object@compartments %>% replace(x)
  return(object)
})

#' @rdname replace
setMethod("replace", signature=c("campsis_model", "compartment_property"), definition=function(object, x) {
  object@compartments <- object@compartments %>% replace(x)
  return(object)
})

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

#' @rdname replace
setMethod("replace", signature=c("campsis_model", "model_statement"), definition=function(object, x) {
  object@model <- object@model %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

#' @rdname replaceAll
setMethod("replaceAll", signature=c("campsis_model", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@model@list <- object@model@list %>% purrr::map(~.x %>% replaceAll(pattern=pattern, replacement=replacement, ...))
  return(object)
})

#' @rdname replaceAll
setMethod("replaceAll", signature=c("campsis_model", "character", "character"), definition=function(object, pattern, replacement, ...) {
  object@model@list <- object@model@list %>% purrr::map(~.x %>% replaceAll(pattern=VariablePattern(pattern), replacement=replacement, ...))
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("campsis_model"), definition=function(object) {
  show(object@model %>% addPropertiesRecords(model=object))
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

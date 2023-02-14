
#_______________________________________________________________________________
#----                             addSuffix                                 ----
#_______________________________________________________________________________

#' Generic function to add a suffix to various objects like parameters, code records,
#' compartment names or a model (all previous objects at the same time).
#' This makes it an extremely powerful function to combine 2 models or more (using function 'add'),
#' that have similar equation, parameter or compartment names.
#' 
#' @param object generic object
#' @param suffix suffix to be appended, single character value
#' @param separator separator to use before the suffix, default is the underscore
#' @param ... extra arguments like 'model' if the changes need to be reflected in the model 
#' @return updated object of the same class as the provided object, unless 'model' was specified, in that case the model is returned
#' @export
#' @rdname addSuffix
addSuffix <- function(object, suffix, separator, ...) {
  stop("No default function is provided")
}

setGeneric("addSuffix", function(object, suffix, separator=NULL, ...) {
  if (!is.character(suffix) && length(suffix) != 1) {
    stop("suffix must be a single character value")
  }
  if (!(grepl(pattern=paste0("^", variablePatternNoStartStr(), "$"), x=suffix))) {
    stop(paste0("suffix '", suffix, "' is not a valid suffix"))
  }
  if (is.null(separator)) {
    separator <- "_" # Default value
  } else {
    if (!is.character(separator) && length(separator) != 1) {
      stop("separator must be a single character value")
    }
    if (!(grepl(pattern=paste0("^", variablePatternNoStartStr(), "$"), x=separator))) {
      stop(paste0("separator '", separator, "' is not a valid separator"))
    }
  }
  standardGeneric("addSuffix")
})

#' @rdname addSuffix
setMethod("addSuffix", signature=c("parameters", "character", "character"), definition=function(object, suffix, separator, ...) {
  args <- list(...)
  model <- args[["model"]]
  
  # Create a new list of parameters
  retValue <- Parameters()
  
  # Add suffix to all parameters and update model code accordingly
  for (parameter in object@list) {
    name <- parameter@name
    if (is.na(name) && is(parameter, "double_array_parameter") && !parameter %>% isDiag()) {
      # Do nothing except adding the parameter
      retValue <- retValue %>% add(parameter)
    } else if (is.na(name)) {
      stop(paste0("Please give a name to ", parameter %>% getNameInModel()))
    } else {
      oldName <- parameter %>% getNameInModel()
      parameter@name <- paste0(name, separator, suffix)
      newName <- parameter %>% getNameInModel()
      retValue <- retValue %>% add(parameter)
      if (!is.null(model)) {
        model <- model %>% replaceAll(pattern=oldName, replacement=newName)
      }
    }
  }
  
  # Return updated parameters or updated model if model was provided
  if (is.null(model)) {
    return(retValue)
  } else {
    model@parameters <- retValue
    return(model)
  }
})

#' @rdname addSuffix
setMethod("addSuffix", signature=c("code_records", "character", "character"), definition=function(object, suffix, separator, ...) {
  args <- list(...)
  model <- args[["model"]]
  
  # Collect all equation names
  equationNames <- object@list %>%
    purrr::map(~getRecordEquationNames(.x)) %>%
    purrr::flatten_chr()
  
  # Get rid of duplicates
  equationNames <- equationNames[!duplicated(equationNames)]
  
  # List of code records
  retValue <- object
  
  # Add suffix
  for (equationName in equationNames) {
    replacementStr <- paste0(equationName, separator, suffix)
    retValue <- retValue %>% 
      replaceAll(pattern=VariablePattern(equationName), replacement=replacementStr)
    
    # Update properties as well (rhs)
    if (!is.null(model)) {
      for (index in seq_len(model@compartments@properties %>% length())) {
        model@compartments@properties@list[[index]]@rhs <- model@compartments@properties@list[[index]]@rhs %>%
          replaceAll(pattern=VariablePattern(equationName), replacement=replacementStr)
      }
    }
  }
  
  # Return updated code record list or updated model if model was provided
  if (is.null(model)) {
    return(retValue)
  } else {
    model@model <- retValue
    return(model)
  }
})

#' @rdname addSuffix
setMethod("addSuffix", signature=c("code_record", "character", "character"), definition=function(object, suffix, separator, ...) {
  retValue <- addSuffix(object=CodeRecords() %>% add(object), suffix=suffix, separator=separator, ...)
  if (is(retValue, "code_records")) {
    retValue <- retValue@list[[1]]
  }
  return(retValue)
})

#' @rdname addSuffix
setMethod("addSuffix", signature=c("compartments", "character", "character"), definition=function(object, suffix, separator, ...) {
  args <- list(...)
  model <- args[["model"]]
  
  # List of compartments
  retValue <- object
  
  # Update compartments
  for (index in seq_len(retValue %>% length())) {
    compartment <- retValue@list[[index]]
    oldName <- compartment %>% toString()
    replacementName <- paste0(oldName, separator, suffix)
    compartment@name <- gsub(pattern="^A_", replacement="", x=replacementName)
    retValue <- retValue %>%
      replace(compartment) # Replacement done by index for compartments
    if (!is.null(model)) {
      model <- model %>% 
        replaceAll(pattern=oldName, replacement=replacementName)
    }
  }
  
  # Return updated code record list or updated model if model was provided
  if (is.null(model)) {
    return(retValue)
  } else {
    model@compartments <- retValue
    return(model)
  }
})

#' @rdname addSuffix
setMethod("addSuffix", signature=c("campsis_model", "character", "character"), definition=function(object, suffix, separator, ...) {
  model <- object
  
  # Add suffix to parameters
  parameters <- model@parameters
  model <- parameters %>% addSuffix(suffix=suffix, separator=separator, model=model)
  
  # Add suffix to equations
  records <- model@model
  model <- records %>% addSuffix(suffix=suffix, separator=separator, model=model)
  
  # Add suffix to ODE compartments
  compartments <- model@compartments
  model <- compartments %>% addSuffix(suffix=suffix, separator=separator, model=model)
  
  # Save properties
  properties <- model@compartments@properties
  
  # Update compartment (properties will be lost)
  model <- model %>% updateCompartments()
  
  # Re-assign properties
  model@compartments@properties <- properties
  
  return(model)
})



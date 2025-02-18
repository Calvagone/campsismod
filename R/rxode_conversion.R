

#' Get code for rxode2
#' 
#' @param model CAMPSIS model
#' @return corresponding model code for rxode2
#' @export
rxodeCode <- function(model) {
  records <- model@model
  properties <- model@compartments@properties
  propertiesCode <- NULL
  if (properties %>% length() > 0) {
    for (property in properties@list) {
      compartmentIndex <- property@compartment
      compartment <- model@compartments %>% find(Compartment(index=compartmentIndex))
      equation <- property %>% toString(model=model, dest="rxode2")
      propertiesCode <- propertiesCode %>% append(equation)
    }
  }
  
  # All records to character vector
  code <- NULL
  for (record in records@list) {
    for (statement in record@statements@list) {
      code <- code %>% append(statement %>% toString(dest="rxode2"))
    }
    if (is(record, "ode_record")) {
      code <- code %>% append(propertiesCode)
    }
  }
  return(code)
}

#' Get the parameters vector for rxode2.
#' 
#' @param model CAMPSIS model
#' @return named vector with THETA values
#' @export
rxodeParams <- function(model) {
  type <- "theta"
  params <- model@parameters
  if (params %>% length() == 0) {
    retValue <- numeric(0)
    names(retValue) <- character(0)
    return(retValue) # Must be named numeric, otherwise rxode2 complains
  }
  maxIndex <- params %>% select("theta") %>% maxIndex()
  
  # Careful, as.numeric(NA) is important...
  # If values are all integers, rxode2 gives a strange error message:
  # Error in rxSolveSEXP(object, .ctl, .nms, .xtra, params, events, inits,  : 
  # when specifying 'thetaMat', 'omega', or 'sigma' the parameters cannot be a 'data.frame'/'matrix'
  
  retValue <- rep(as.numeric(NA), maxIndex)
  names <- rep("", maxIndex)
  
  for (i in seq_len(maxIndex)) {
    param <- params %>% getByIndex(Theta(index=i))
    if (length(param) == 0) {
      stop(paste0("Missing param ", i, "in ", type, " vector"))
    } else {
      retValue[i] <- param@value
      names[i] <- param %>% getNameInModel()
    }
  }
  names(retValue) <- names
  
  return(retValue)
}

#' Get the OMEGA/SIGMA matrix for rxode2.
#' 
#' @param model Campsis model or Campsis parameters
#' @param type either omega or sigma
#' @return omega/sigma named matrix
#' @export
rxodeMatrix <- function(model, type="omega") {
  
  if (is(model, "campsis_model")) {
    subset <- model@parameters %>%
      select(type)
  } else if (is(model, "parameters")) {
    subset <- model %>%
      select(type)
  } else {
    stop("model must be either a Campsis model or a parameters object")
  }

  if (subset %>% length()==0) {
    return(matrix(data=numeric(0), nrow=0, ncol=0))
  }
  
  # Standardise parameters
  subset <- subset %>%
    standardise()
  
  # Retrieve max index
  maxIndex <- subset %>%
    maxIndex()
  matrix <- matrix(0L, nrow=maxIndex, ncol=maxIndex)
  names <- rep("", maxIndex)
  
  # Fill in matrix
  for (elem in subset@list) {
    matrix[elem@index, elem@index2] <- elem@value
    matrix[elem@index2, elem@index] <- elem@value
    if (elem@index==elem@index2) {
      names[elem@index] <- elem %>% getNameInModel()
    }
  }
  
  assertthat::assert_that(all(names != ""),
                          msg=sprintf("At least one %s is missing.", toupper(type)))

  rownames(matrix) <- names
  colnames(matrix) <- names
  return(matrix)
}


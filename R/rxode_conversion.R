

#' Get code for RxODE.
#' 
#' @param model PMX model
#' @return code for RxODE
#' @export
rxodeCode <- function(model) {
  records <- model@model
  properties <- model@compartments@properties
  
  if (properties %>% length() > 0) {
    odeRecord <- records %>% getByName("ODE")
    if (length(odeRecord) == 0) {
      stop("Not able to add compartment properties for RxODE: no ODE block")
    }
    for (property in properties@list) {
      compartmentIndex <- property@compartment
      compartment <- model@compartments %>% getByIndex(Compartment(index=compartmentIndex))
      equation <- property %>% toString(model=model, dest="RxODE")
      odeRecord@code <- odeRecord@code %>% append(equation)
    }
    records <- records %>% replace(odeRecord)
  }
  
  # All records to 'single' record
  code <- NULL
  for (record in records@list) {
    code <- c(code, record@code)
  }
  return(code)
}

#' Get the parameters vector for RxODE.
#' 
#' @param model PMX model
#' @return named vector with THETA values
#' @export
rxodeParams <- function(model) {
  type <- "theta"
  params <- model@parameters
  if (params %>% length() == 0) {
    retValue <- numeric(0)
    names(retValue) <- character(0)
    return(retValue) # Must be named numeric, otherwise RxODE complains
  }
  maxIndex <- params %>% maxIndex(type=type)
  
  # Careful, as.numeric(NA) is important...
  # If values are all integers, RxODE gives a strange error message:
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

#' Get the matrix for RxODE.
#' 
#' @param model PMX model
#' @param type either omega or sigma
#' @return named matrix
#' @export
rxodeMatrix <- function(model, type="omega") {
  
  # Make sure parameters are standardised
  params <- model@parameters %>% standardise()
  
  if (params %>% select(type) %>% length()==0) {
    return(matrix(data = numeric(0)))
  }
  maxIndex <- params %>% maxIndex(type=type)
  matrix <- matrix(0L, nrow=maxIndex, ncol=maxIndex)
  names <- rep("", maxIndex)
  
  if (type=="omega") {
    mockParam1 <- Omega(index=1, index2=1)
    mockParam2 <- Omega(index=1, index2=1)
  } else {
    mockParam1 <- Sigma(index=1, index2=1)
    mockParam2 <- Sigma(index=1, index2=1)
  }
  
  for (i in seq_len(maxIndex)) {
    mockParam1@index <- i
    mockParam2@index2 <- i
    
    for (j in seq_len(maxIndex)) {
      mockParam2@index <- j
      mockParam1@index2 <- j
      
      param <- params %>% getByIndex(mockParam1)
      if (length(param) == 0) {
        param <- params %>% getByIndex(mockParam2)
      }
      if (length(param) == 0) {
        matrix[i, j] <- 0
      } else {
        matrix[i, j] <- param@value
      }
    } 
  }
  for (i in seq_len(maxIndex)) {
    mockParam1@index <- i
    mockParam1@index2 <- i
    param <- params %>% getByIndex(mockParam1)
    if (length(param) == 0) {
      stop(paste0("Missing param ", i, "in ", type, " matrix"))
    } else {
      names[i] <- param %>% getNameInModel()
    }
  }
  rownames(matrix) <- names
  colnames(matrix) <- names
  return(matrix)
}


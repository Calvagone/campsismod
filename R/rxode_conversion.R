

#' Get code for RxODE.
#' 
#' @param pmxmod PMX model
#' @return code for RxODE
#' @export
rxodeCode <- function(pmxmod) {
  model <- pmxmod@model
  code <- NULL
  for (record in model@list) {
    code <- c(code, record@code)
  }
  return(code)
}

#' Get the parameters vector for RxODE.
#' 
#' @param pmxmod PMX model
#' @return named vector with THETA values
#' @export
rxodeParams <- function(pmxmod) {
  type <- "theta"
  params <- pmxmod@parameters
  maxIndex <- params %>% maxIndex(type=type)
  retValue <- rep(NA, maxIndex)
  names <- rep("", maxIndex)
  
  for (i in seq_len(maxIndex)) {
    param <- params %>% getByIndex(Theta(index=i))
    if (length(param) == 0) {
      stop(paste0("Missing param ", i, "in ", type, " vector"))
    } else {
      retValue[i] <- param@value
      names[i] <- param %>% getName()
    }
  }
  names(retValue) <- names
  
  return(retValue)
}

#' Get the matrix for RxODE.
#' 
#' @param pmxmod PMX model
#' @param type either omega or sigma
#' @return named matrix
#' @export
rxodeMatrix <- function(pmxmod, type="omega") {
  
  params <- pmxmod@parameters
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
      names[i] <- param %>% getName()
    }
  }
  rownames(matrix) <- names
  colnames(matrix) <- names
  return(matrix)
}


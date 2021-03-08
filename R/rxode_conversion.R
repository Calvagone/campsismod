

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
    param <- params %>% getByIndex(type=type, index=i)
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
  
  for (i in seq_len(maxIndex)) {
    for (j in seq_len(maxIndex)) {
      param <- params %>% getByIndex(type=type, index=i, index2=j)
      if (length(param) == 0) {
        param <- params %>% getByIndex(type=type, index=j, index2=i)
      }
      if (length(param) == 0) {
        matrix[i, j] <- 0
      } else {
        matrix[i, j] <- param@value
      }
    } 
  }
  for (i in seq_len(maxIndex)) {
    param <- params %>% getByIndex(type=type, index=i, index2=i)
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


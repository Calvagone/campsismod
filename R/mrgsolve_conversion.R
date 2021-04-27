
#' Get the parameters block for mrgsolve.
#' 
#' @param model PMX model
#' @return character vector, each value is a line or character(0) if no param
#' @export
mrgsolveParam <- function(model) {
  params <- rxodeParams(model)
  if (params %>% length()==0) {
    return(character(0))
  }
  retValue <- "[PARAM] @annotated"
  for (index in seq_len(length(params))) {
    param <- params[index]
    retValue <- retValue %>% append(paste0(names(param), " : ", as.numeric(param), " : ", names(param)))
  }
  return(retValue)
}

#' Get the compartment block for mrgsolve.
#' 
#' @param model PMX model
#' @return character vector, each value is a line
#' @export
mrgsolveCompartment <- function(model) {
  compartments <- model@compartments
  retValue <- "[CMT] @annotated"
  for (compartment in compartments@list) {
    retValue <- retValue %>% append(paste0(compartment %>% getName(), " : ", compartment@name))
  }
  return(retValue)
}

#' Get the OMEGA/SIGMA matrix for mrgsolve.
#' 
#' @param model PMX model
#' @param type either omega or sigma
#' @return named matrix or character(0) if matrix is empty
#' @export
mrgsolveMatrix <- function(model, type="omega") {
  matrix <- rxodeMatrix(model, type=type)
  if (nrow(matrix) == 0) {
    return(character(0))
  }
  if (type=="omega") {
    retValue <- "[OMEGA] @annotated @block"
  } else {
    retValue <- "[SIGMA] @annotated @block"
  }
  names <- row.names(matrix)
  for (rowIndex in seq_len(nrow(matrix))) {
    retValue <- retValue %>% append(paste0(names[rowIndex], " : ",
                                    paste0(matrix[rowIndex, seq_len(rowIndex)], collapse=" "), " : ",
                                    names[rowIndex]))
  }  
  return(retValue)
}

#' Get the MAIN block for mrgsolve.
#' 
#' @param model PMX model
#' @return MAIN block
#' @export
mrgsolveMain <- function(model) {
  records <- model@model
  properties <- model@compartments@properties
  retValue <- "[MAIN]"
  record <- records %>% getByName("MAIN")
  retValue <- mrgsolveBlock(record, init="[MAIN]")
  if (properties %>% length() > 0) {
    for (property in properties@list) {
      compartmentIndex <- property@compartment
      compartment <- model@compartments %>% getByIndex(Compartment(index=compartmentIndex))
      equation <- paste0(property %>% toString(model=model, dest="mrgsolve"), ";")
      retValue <- retValue %>% append(equation)
    }
  }
  return(retValue)
}

#' Convert code record for mrgsolve.
#' 
#' @param record code record
#' @param init name of mrgsolve block
#' @param capture 'capture' instead of 'double'
#' @return translated record for mrgsolve
#' @export
mrgsolveBlock <- function(record, init=NULL, capture=FALSE) {
  retValue <- init
  if (length(record) == 0) {
    return(retValue)
  }
  for (index in seq_len(length(record@code))) {
    line <- record@code[index]
    if (isODE(line)) {
      name <- extractTextBetweenBrackets(line)
      rhs <- extractRhs(line)
      line <- paste0("dxdt_", name, "=", rhs, ";")
    } else if (isEquation(line)) {
      if (capture) {
        line <- paste0("capture ", line, ";")
      } else {
        line <- paste0("double ", line, ";")
      }
    } else {
      line <- paste0(line, ";")
    }
    retValue <- retValue %>% append(line)
  }
  return(retValue)
}

#' Get the ODE block for mrgsolve.
#' 
#' @param model PMX model
#' @return ODE block
#' @export
mrgsolveOde <- function(model) {
  records <- model@model
  odeRecord <- records %>% getByName("ODE")
  retValue <- mrgsolveBlock(odeRecord, init="[ODE]")
  return(retValue)
}

#' Get the TABLE block for mrgsolve.
#'
#' @param model PMX model
#' @return TABLE block
#' @export
mrgsolveTable <- function(model) {
  records <- model@model
  errorRecord <- records %>% getByName("ERROR")
  retValue <- mrgsolveBlock(errorRecord, init="[TABLE]", capture=TRUE)
  return(retValue)
}

#' Get the CAPTURE block for mrgsolve.
#'
#' @param outvars outvars from pmxsim
#' @param model PMX model
#' @return CAPTURE block or character(0) if no variable in outvars
#' @export
mrgsolveCapture <- function(outvars, model) {
  # Get rid of variables that are already in error block (and thus already captured in TABLE)
  outvars <- convertOutvarsToCapture(outvars, model=model)
  
  if (is.null(outvars) || outvars %>% length()==0) {
    return(character(0))
  } else {
    return(paste("[CAPTURE]", outvars))
  }
}

#' Convert outvars argument to capture. Variables that are already in error block
#' will be discarded.
#'
#' @param outvars character vector
#' @param model PMX model
#' @return all variables to capture
#'
convertOutvarsToCapture <- function(outvars, model) {
  # List all variables that are already exported into mrgsolve TABLE block by pmxmod
  error <- model@model %>% getByName("ERROR")
  list <- NULL
  if (length(error) > 0) {
    for (line in error@code) {
      if (isEquation(line)) {
        lhs <- extractLhs(line) %>% trim()
        list <- list %>% append(lhs)
      }
    }
    outvars <- outvars[!(outvars %in% list)]
  }
  return(outvars)
}

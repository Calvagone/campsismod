
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
  characteristics <- model@compartments@characteristics
  initial_conditions <- model@compartments@initial_conditions
  retValue <- "[MAIN]"
  record <- records %>% getByName("PK")
  retValue <- mrgsolveBlock(record, init="[MAIN]")
  if (characteristics %>% length() > 0) {
    for (characteristic in characteristics@list) {
      compartmentIndex <- characteristic@compartment
      compartment <- model@compartments %>% getByIndex(Compartment(index=compartmentIndex))
      equation <- paste0(characteristic %>% getPrefix(dest="mrgsolve"), "_", compartment %>% getName(), "=", characteristic@rhs, ";")
      retValue <- retValue %>% append(equation)
    }
  }
  if (initial_conditions %>% length() > 0) {
    for (initial_condition in initial_conditions@list) {
      equation <- paste0(initial_condition %>% toString(model=model, dest="mrgsolve"), ";")
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
  desRecord <- records %>% getByName("DES")
  retValue <- mrgsolveBlock(desRecord, init="[ODE]")
  return(retValue)
}

#' Get the TABLE block for mrgsolve.
#'
#' @param model PMX model
#' @return TABLE block
#' @export
mrgsolveTable <- function(model) {
  records <- model@model
  characteristics <- model@compartments@characteristics
  errorRecord <- records %>% getByName("ERROR")
  retValue <- mrgsolveBlock(errorRecord, init="[TABLE]", capture=TRUE)
  return(retValue)
}

#' Get the CAPTURE block for mrgsolve.
#'
#' @param outvars outvars from pmxsim
#' @return CAPTURE block or character(0) if no variable in outvars
#' @export
mrgsolveCapture <- function(outvars) {
  if (is.null(outvars) || outvars %>% length()==0) {
    return(character(0))
  } else {
    return(paste("[CAPTURE]", outvars))
  }
}
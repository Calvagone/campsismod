
#' Get the parameters block for mrgsolve.
#' 
#' @param model CAMPSIS model
#' @return character vector, 1 parameter per line. First one is header [PARAM].
#' @export
mrgsolveParam <- function(model) {
  params <- rxodeParams(model)
  retValue <- "[PARAM] @annotated"
  for (index in seq_len(length(params))) {
    param <- params[index]
    retValue <- retValue %>% append(paste0(names(param), " : ", as.numeric(param), " : ", names(param)))
  }
  return(retValue)
}

#' Get the compartment block for mrgsolve.
#' 
#' @param model CAMPSIS model
#' @return character vector, each value is a line
#' @export
mrgsolveCompartment <- function(model) {
  compartments <- model@compartments
  retValue <- "[CMT] @annotated"
  for (compartment in compartments@list) {
    retValue <- retValue %>% append(paste0(compartment %>% toString(), " : ", compartment@name))
  }
  return(retValue)
}

#' Get the OMEGA/SIGMA matrix for mrgsolve.
#' 
#' @param model CAMPSIS model
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
#' @param model CAMPSIS model
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
      compartment <- model@compartments %>% find(Compartment(index=compartmentIndex))
      equation <- paste0(property %>% toString(model=model, dest="mrgsolve"), ";")
      retValue <- retValue %>% append(equation)
    }
  }
  return(retValue)
}

#' Convert CAMPSIS comment style to C/C++ code.
#' Only the first # is translated to //.
#' 
#' @param x any record line
#' @return same line with comments translated to C/C++
#' @keywords internal
convertAnyComment <- function(x) {
  return(sub(pattern="#", replacement="//", x=x))
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
  if (is.null(record)) {
    return(retValue)
  }
  for (statement in record@statements@list) {
    retValue <-
      retValue %>% append(statement %>% toString(
        dest = "mrgsolve",
        init = !capture,
        capture = capture
      ))
  }
  return(retValue)
}

#' Get the ODE block for mrgsolve.
#' 
#' @param model CAMPSIS model
#' @return ODE block
#' @export
mrgsolveOde <- function(model) {
  records <- model@model
  odeRecord <- records %>% getByName("ODE")
  
  # Automatically replace simulation time 't' (default in CAMPSIS) by SOLVERTIME
  if (!is.null(odeRecord)) {
    odeRecord <- odeRecord %>% campsismod::replaceAll(pattern=VariablePattern("t"), replacement="SOLVERTIME")
  }
  retValue <- mrgsolveBlock(odeRecord, init="[ODE]")
  return(retValue)
}

#' Get the TABLE block for mrgsolve.
#'
#' @param model CAMPSIS model
#' @return TABLE block if at least one line in error record, character(0) otherwise
#' @export
mrgsolveTable <- function(model) {
  records <- model@model
  errorRecord <- records %>% getByName("ERROR")
  if (is.null(errorRecord)) {
    return(character(0))
  }
  retValue <- mrgsolveBlock(errorRecord, init="[TABLE]", capture=TRUE)
  return(retValue)
}

#' Get the CAPTURE block for mrgsolve.
#'
#' @param outvars outvars in method simulate
#' @param model CAMPSIS model
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
#' @param model CAMPSIS model
#' @importFrom purrr keep map_chr
#' @return all variables to capture
#' @keywords internal
convertOutvarsToCapture <- function(outvars, model) {
  # List all variables that are already exported into mrgsolve TABLE block by pmxmod
  error <- model@model %>% getByName("ERROR")
  list <- NULL
  if (!is.null(error)) {
    list <- error@statements@list %>% purrr::keep(~is(.x, "equation")) %>% purrr::map_chr(~.x@lhs)
    outvars <- outvars[!(outvars %in% list)]
  }
  return(outvars)
}


#' Get the parameters block for mrgsolve.
#' 
#' @param model PMX model
#' @return character vector, each value is a line
#' @export
mrgsolveParam <- function(model) {
  params <- rxodeParams(model)
  retValue <- "[PARAM]"
  for (index in seq_len(length(params))) {
    param <- params[index]
    retValue <- retValue %>% append(paste0(names(param), " : ", as.numeric(param)))
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
  retValue <- "[CMT]"
  for (compartment in compartments@list) {
    retValue <- retValue %>% append(compartment %>% getName())
  }
  return(retValue)
}

#' Get the OMEGA/SIGMA matrix for mrgsolve.
#' 
#' @param model PMX model
#' @param type either omega or sigma
#' @return named matrix
#' @export
mrgsolveMatrix <- function(model, type="omega") {
  matrix <- rxodeMatrix(model, type=type)
  if (type=="omega") {
    retValue <- "[OMEGA]"
  } else {
    retValue <- "[SIGMA]"
  }
  names <- row.names(matrix)
  for (rowIndex in seq_len(nrow(matrix))) {
    retValue <- retValue %>% append(paste0(names[rowIndex], " : ",
                                    paste0(matrix[rowIndex, seq_len(rowIndex)], collapse=" ")))
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
  retValue <- "[MAIN]"
  record <- records %>% getByName("PK")
  return(mrgsolveBlock(record, init="[MAIN]"))
}

#' Convert code record for mrgsolve.
#' 
#' @param record code record
#' @param init name of mrgsolve block
#' @return translated record for mrgsolve
#' @export
mrgsolveBlock <- function(record, init=NULL) {
  retValue <- init
  if (length(record) == 0) {
    return(retValue)
  }
  for (index in seq_len(length(record@code))) {
    line <- record@code[index]
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
  characteristics <- model@compartments@characteristics
  desRecord <- records %>% getByName("DES")
  errorRecord <- records %>% getByName("ERROR")

  retValue <- mrgsolveBlock(desRecord, init="[ODE]")
  retValue <- retValue %>% append(mrgsolveBlock(errorRecord))
  
  return(retValue)
}
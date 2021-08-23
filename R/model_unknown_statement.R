#_______________________________________________________________________________
#----                     unknown_statement class                           ----
#_______________________________________________________________________________

validateUnknownStatement <- function(object) {
  return(expectOne(object, "line"))
}

#' 
#' Unknown statement class. Any statement not recognized by campsismod.
#' 
#' @export
setClass(
  "unknown_statement",
  representation(
    line = "character"
  ),
  contains = "model_statement",
  validity = validateUnknownStatement
)

#' 
#' Create a new ordinary differential equation (ODE).
#' 
#' @param line line which was not recognised
#' @param comment comment if any, single character string
#' @return an unknown statement
#' @export
UnknownStatement <- function(line, comment=as.character(NA)) {
  return(new("unknown_statement", line=line, comment=comment))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("unknown_statement"), definition = function(x) {
  return(as.character(NA)) # unknown statement non-identifiable 
})
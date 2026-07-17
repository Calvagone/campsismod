#_______________________________________________________________________________
#----                     unknown_statement class                           ----
#_______________________________________________________________________________

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
  validity = function(object) {
    return(expectOne(object, "line"))
  }
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

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

#' @rdname replace_all
setMethod("replace_all", signature=c("unknown_statement", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@line <- object@line %>% replace_all(pattern=pattern, replacement=replacement, ...)
  return(object)
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature=c("unknown_statement"), definition=function(object, ...) {
  args <- list(...)
  dest <- process_extra_arg(args=args, name="dest", default="campsis")
  show <- process_extra_arg(args=args, name="show", default=FALSE)

  if (dest=="campsis") {
    if (show) {
      retValue <- paste0("[UNKNOWN STATEMENT] ", object@line)
    } else {
      retValue <- object@line
    }
  } else if (isRxODE(dest) || dest=="mrgsolve" || dest=="NONMEM") {
    retValue <- object@line
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})

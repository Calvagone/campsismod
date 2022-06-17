#_______________________________________________________________________________
#----                         line_break class                              ----
#_______________________________________________________________________________

#' 
#' Line-break class. A linebreak in the model.
#' 
#' @export
setClass(
  "line_break",
  representation(
  ),
  contains = "model_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new line break.
#' 
#' @return a line break
#' @export
LineBreak <- function() {
  return(new("line_break"))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("line_break"), definition = function(x) {
  return(as.character(NA)) # line_break non-identifiable 
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("line_break"), definition=function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="campsis")
  if (dest=="campsis" || dest=="RxODE" || dest=="mrgsolve" || dest=="NONMEM") {
    retValue <- ""
  } else {
    UnsupportedDestException()
  }
  return(retValue)
})
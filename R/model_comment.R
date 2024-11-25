
#_______________________________________________________________________________
#----                           comment class                               ----
#_______________________________________________________________________________

#' 
#' Comment class. A statement starting with #.
#' 
#' @export
setClass(
  "comment",
  representation(
  ),
  contains = "model_statement",
  validity = function(object) {
    return(TRUE)
  }
)

#' 
#' Create a new comment.
#' 
#' @param x comment, single character string
#' @return a comment
#' @export
Comment <- function(x) {
  return(new("comment", comment=x))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("comment"), definition = function(x) {
  return(paste0("COMMENT (", x@comment, ")"))
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("comment"), definition=function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="campsis")
  if (dest=="campsis" || isRxODE(dest) || dest=="mrgsolve" || dest=="NONMEM") {
    retValue <- ""
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})

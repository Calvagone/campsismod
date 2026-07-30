
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
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("comment"), definition = function(x) {
  return(paste0("COMMENT (", x@comment, ")"))
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature=c("comment"), definition=function(object, ...) {
  dest <- process_extra_arg(args=list(...), name="dest", default="campsis")
  if (dest=="campsis" || is_rxode(dest) || dest=="mrgsolve" || dest=="NONMEM") {
    retValue <- ""
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% append_comment(object, dest))
})

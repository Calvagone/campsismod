
#_______________________________________________________________________________
#----                           comment class                               ----
#_______________________________________________________________________________

validateComment <- function(object) {
  return(TRUE)
}

#' 
#' Comment class. A statement starting with #.
#' 
#' @slot comment a comment associated to this model statement
#' @export
setClass(
  "comment",
  representation(
  ),
  contains = "model_statement",
  validity = validateComment
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
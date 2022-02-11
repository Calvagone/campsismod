
#_______________________________________________________________________________
#----                       model_statement class                           ----
#_______________________________________________________________________________

validateModelStatement <- function(object) {
  return(expectOne(object, c("comment")))
}

#' 
#' Model statement class. Any statement in a code record.
#' 
#' @slot comment a comment associated to this model statement
#' @export
setClass(
  "model_statement",
  representation(
    comment = "character"
  ),
  contains = "pmx_element",
  prototype = prototype(comment=as.character(NA)),
  validity = validateModelStatement
)

#' 
#' Append comment.
#' 
#' @param str single character string
#' @param object model statement
#' @param dest destination engine, string
#' @return single character string followed by comment
#' @keywords internal
appendComment <- function(str, object, dest) {
  comment <- object@comment
  if (is.na(comment)) {
    return(str)
  } else {
    if (dest=="mrgsolve") {
      symbol <- "//"
    } else if (dest=="NONMEM") {
      symbol <- ";"
    } else {
      symbol <- "#" # Both campsis and RxODE
    }
    if (str=="") {
      return(paste0(symbol, " ", comment))
    } else {
      return(paste0(str, " ", symbol, " ", comment))
    }
  }
}

#' 
#' Unknown destination engine exception.
#'
#' @keywords internal
#' @return no return value
#' 
UnsupportedDestException <- function() {
  stop("Only RxODE, mrgsolve or campsis are supported")
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("model_statement"), definition=function(object) {
  cat(object %>% toString(show=TRUE))
})

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

#' @rdname replaceAll
setMethod("replaceAll", signature=c("model_statement", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  return(object)
})


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
appendComment <- function(str, object, dest) {
  if (is.na(object@comment)) {
    return(str)
  } else {
    symbol <- ifelse(dest=="mrgsolve", "//", "#")
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
UnsupportedDestException <- function() {
  stop("Only RxODE, mrgsolve or campsis are supported")
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________


setMethod("show", signature=c("model_statement"), definition=function(object) {
  cat(object %>% toString())
})

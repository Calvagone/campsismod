
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
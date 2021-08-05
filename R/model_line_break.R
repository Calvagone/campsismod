#_______________________________________________________________________________
#----                         line_break class                              ----
#_______________________________________________________________________________

validateLineBreak <- function(object) {
  return(TRUE)
}

#' 
#' Line-break class. A linebreak in the model.
#' 
#' @export
setClass(
  "line_break",
  representation(
  ),
  contains = "model_statement",
  validity = validateLineBreak
)
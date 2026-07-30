#_______________________________________________________________________________
#----                         line_break class                              ----
#_______________________________________________________________________________

#'
#' Line-break class. A linebreak in the model.
#'
#' @export
setClass(
  "line_break",
  representation(),
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
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("line_break"), definition = function(x) {
  return(as.character(NA)) # line_break non-identifiable
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature = c("line_break"), definition = function(object, ...) {
  dest <- process_extra_arg(args = list(...), name = "dest", default = "campsis")
  if (dest == "campsis" || is_rxode(dest) || dest == "mrgsolve" || dest == "NONMEM") {
    retValue <- ""
  } else {
    UnsupportedDestException()
  }
  return(retValue)
})

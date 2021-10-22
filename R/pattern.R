#_______________________________________________________________________________
#----                           pattern class                               ----
#_______________________________________________________________________________

validatePattern <- function(object) {
  if (object %>% as.character() %>% length()!=1) {
    return("Invalid pattern. Pattern must be a single character string.")
  }
  return(TRUE)
}

#' 
#' Pattern class.
#' 
#' @export
setClass(
  "pattern",
  representation(
  ),
  contains="character",
  validity=validatePattern
)

#' 
#' Create a pattern.
#' 
#' @param x regular expression
#' @return a pattern
#' @export
Pattern <- function(x) {
  return(new("pattern", x))
}

#_______________________________________________________________________________
#----                         variable_pattern class                        ----
#_______________________________________________________________________________

#' 
#' Variable pattern class.
#' 
#' @export
setClass(
  "variable_pattern",
  representation(
  ),
  contains="pattern"
)

#' 
#' Create a variable pattern.
#' 
#' @param x variable name
#' @return a pattern
#' @export
VariablePattern <- function(x) {
  return(new("variable_pattern", x))
}

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

#' @rdname replaceAll
setMethod("replaceAll", signature=c("character", "variable_pattern", "character"), definition=function(object, pattern, replacement, ...) {
  return(gsub(paste0("([^a-zA-Z0-9_]|^)(", pattern, ")([^a-zA-Z0-9_]|$)"), replacement=paste0("\\1", replacement, "\\3"), x=object))
})

#' @rdname replaceAll
setMethod("replaceAll", signature=c("character", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  return(gsub(pattern, replacement=replacement, x=object))
})

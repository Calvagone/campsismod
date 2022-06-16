#_______________________________________________________________________________
#----                         pmx_position class                            ----
#_______________________________________________________________________________

#' 
#' PMX position class.
#' 
#' @export
setClass(
  "pmx_position",
  representation(
    undefined = "logical",
    by_index = "logical",
    by_element = "logical",
    after = "logical"
  ),
  prototype=prototype(after=TRUE),
  validity=function(object) {
    check1 <- expectOne(object, "undefined")
    check2 <- expectOne(object, "by_index")
    check3 <- expectOne(object, "by_element")
    check4 <- expectOne(object, "after")
    return(c(check1, check2, check3, check4))
  }
)

#_______________________________________________________________________________
#----                     pmx_undefined_position class                      ----
#_______________________________________________________________________________

#' 
#' PMX undefined position class.
#' 
#' @export
setClass(
  "pmx_undefined_position",
  representation(
  ),
  contains="pmx_position"
)

#' 
#' Undefined position.
#' 
#' @return an undefined position object
#' @export
#' @keywords internal
UndefinedPosition <- function() {
  return(new("pmx_undefined_position", undefined=TRUE, by_index=FALSE, by_element=FALSE))
}

#_______________________________________________________________________________
#----                      pmx_position_by_index class                      ----
#_______________________________________________________________________________

#' 
#' PMX position by index class.
#' 
#' @export
setClass(
  "pmx_position_by_index",
  representation(
    index = "integer"
  ),
  contains="pmx_position",
  validity=function(object) {
    check <- expectOne(object, "index")
    return(check)
  }
)

#_______________________________________________________________________________
#----                     pmx_position_by_element class                     ----
#_______________________________________________________________________________

#' 
#' PMX position by element class.
#' 
#' @export
setClass(
  "pmx_position_by_element",
  representation(
    element = "pmx_element"
  ),
  contains="pmx_position"
)

#' 
#' Element position in list.
#' 
#' @param x either an integer position (useful to add an element in a code record at a specified position) or
#'  an model element (element can be a model statement or a code record)
#' @param after element to be added will be added after x (if after is TRUE) or before x (if after is FALSE)
#' @return a position object
#' @export
Position <- function(x, after=TRUE) {
  if (is(x, "pmx_element")) {
    retValue <- new("pmx_position_by_element", element=x, after=after, undefined=FALSE, by_index=FALSE, by_element=TRUE)
  } else if (is.numeric(x)) {
    retValue <- new("pmx_position_by_index", index=as.integer(x), after=after, undefined=FALSE, by_index=TRUE, by_element=FALSE)
  } else {
    stop("x can only be a PMX element or an integer position")
  }
  return(retValue)
}

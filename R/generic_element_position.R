#_______________________________________________________________________________
#----                         pmx_position class                            ----
#_______________________________________________________________________________

validatePmxPosition <- function(object) {
  check1 <- expectOne(object, "undefined")
  check2 <- expectOne(object, "by_index")
  check3 <- expectOne(object, "by_element")
  check4 <- expectOne(object, "after")
  return(c(check1, check2, check3, check4))
}

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
  validity=validatePmxPosition
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
#' @export
UndefinedPosition <- function() {
  return(new("pmx_undefined_position", undefined=TRUE, by_index=FALSE, by_element=FALSE))
}

#_______________________________________________________________________________
#----                      pmx_position_by_index class                      ----
#_______________________________________________________________________________

validatePmxPositionByIndex <- function(object) {
  check <- expectOne(object, "index")
  return(check)
}

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
  validity=validatePmxPositionByIndex
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
#' @param x either an integer position or an list element
#' @param after element to be added will be added after x (if after is TRUE) or before x (if after is FALSE)
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
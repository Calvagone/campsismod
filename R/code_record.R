
checkCodeRecord <- function(object) {
  return(expectZeroOrMore(object, "code"))
}

setClass(
  "code_record",
  representation(
    code = "character"
  ),
  contains = "pmx_element",
  validity = checkCodeRecord
)

#_______________________________________________________________________________
#----                            PK record                                  ----
#_______________________________________________________________________________

#' @export
setClass(
  "pk_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create PK code record.
#' 
#' @param code code record
#' @export
PkRecord <- function(code=character()) {
  return(new("pk_record", code=code))
}

#_______________________________________________________________________________
#----                           PRED record                                 ----
#_______________________________________________________________________________

#' @export
setClass(
  "pred_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create PRED code record.
#' 
#' @param code code record
#' @export
PredRecord <- function(code=character()) {
  return(new("pred_record", code=code))
}

#_______________________________________________________________________________
#----                            DES record                                 ----
#_______________________________________________________________________________

#' @export
setClass(
  "des_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create DES code record.
#' 
#' @param code code record
#' @export
DesRecord <- function(code=character()) {
  return(new("des_record", code=code))
}

#_______________________________________________________________________________
#----                           ERROR record                                ----
#_______________________________________________________________________________


#' @export
setClass(
  "error_record",
  representation(
  ),
  contains = "code_record"
)

#' 
#' Create ERROR code record.
#' 
#' @param code code record
#' @export
ErrorRecord <- function(code=character()) {
  return(new("error_record", code=code))
}

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("pk_record"), definition=function(x) {
  return("PK")
})

setMethod("getName", signature=c("pred_record"), definition=function(x) {
  return("PRED")
})

setMethod("getName", signature=c("des_record"), definition=function(x) {
  return("DES")
})

setMethod("getName", signature=c("error_record"), definition=function(x) {
  return("ERROR")
})


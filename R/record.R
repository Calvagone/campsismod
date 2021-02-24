
setClass(
  "record",
  representation(
    code = "character"
  )
)

#' PK record class.
#' 
#' @export
setClass(
  "pk_record",
  representation(
  ),
  contains = "record"
)

#' PRED record class.
#' 
#' @export
setClass(
  "pred_record",
  representation(
  ),
  contains = "record"
)

#' DES record class.
#' 
#' @export
setClass(
  "des_record",
  representation(
  ),
  contains = "record"
)

#' ERROR record class.
#' 
#' @export
setClass(
  "error_record",
  representation(
  ),
  contains = "record"
)

#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

setMethod("getName", signature=c("pk_record"), definition=function(object) {
  return("PK")
})

setMethod("getName", signature=c("pred_record"), definition=function(object) {
  return("PRED")
})

setMethod("getName", signature=c("des_record"), definition=function(object) {
  return("DES")
})

setMethod("getName", signature=c("error_record"), definition=function(object) {
  return("ERROR")
})


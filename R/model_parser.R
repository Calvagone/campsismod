
#' @rdname parseRecord
setMethod("parseRecord", signature=c("statement_record"), definition=function(object) {
  return(object)
})

#' @rdname parseRecord
setMethod("parseRecord", signature=c("code_record"), definition=function(object) {
  return(object)
})
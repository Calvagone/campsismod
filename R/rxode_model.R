
setClass(
  "rxode_model",
  representation(
    code = "character",
    theta = "numeric",
    omega = "matrix",
    sigma = "matrix"
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @rdname export
setMethod("export", signature=c("pmx_model", "rxode_type"), definition=function(object, dest, ...) {
  return(
    new(
      "rxode_model",
      code = rxodeCode(object),
      theta = rxodeParams(object),
      omega = rxodeMatrix(object, type = "omega"),
      sigma = rxodeMatrix(object, type = "sigma")
    )
  )
})

setClass(
  "pmx_model",
  representation(
    code = "character",       # Mandatory
    parameters = "parameters" # Mandatory    
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Export function.
#' 
#' @param object generic object
#' @param dest destination
#' @return specific object depending on given destination
#' @export
export <- function(object, dest) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest) {
  standardGeneric("export")
})

setMethod("export", signature=c("pmx_model", "character"), definition=function(object, dest) {
  if (dest=="RxODE") {
    return(
      new(
        "rxode_model",
        code = object@code,
        theta = rxodeParams(object),
        omega = rxodeMatrix(object, type = "omega"),
        sigma = rxodeMatrix(object, type = "sigma")
      )
    )
  } else {
    stop("Only RxODE is supported for now")
  }
})
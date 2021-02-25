
#_______________________________________________________________________________
#----                              getName                                  ----
#_______________________________________________________________________________

#' Get name in model code.
#' 
#' @param object generic object
#' @return name
#' @export
getName <- function(object) {
  stop("No default function is provided")
}

setGeneric("getName", function(object) {
  standardGeneric("getName")
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write generic object.
#' 
#' @param object generic object
#' @param file path of the output dir or ZIP filename
#' @param ... extra arguments
#' @export
write <- function(object, file, ...) {
  stop("No default function is provided")
}

setGeneric("write", function(object, file, ...) {
  standardGeneric("write")
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Read generic object.
#' 
#' @param file path where to read the file
#' @param ... extra arguments
#' @export
read <- function(file, ...) {
  stop("No default function is provided")
}

setGeneric("read", function(file, ...) {
  standardGeneric("read")
})

#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

setClass(
  "rxode_type",
  representation(
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

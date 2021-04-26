
#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' Disable.
#' 
#' @param object generic object
#' @param x what needs to be disabled
#' @param ... extra arguments needed for disabling
#' @return standardised object
#' @export
disable <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("disable", function(object, x, ...) {
  standardGeneric("disable")
})

#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' Export type class.
#' 
#' @export
setClass(
  "export_type",
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
#' @param ... optional arguments
#' @return specific object depending on given destination
#' @export
export <- function(object, dest, ...) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest, ...) {
  standardGeneric("export")
})

#_______________________________________________________________________________
#----                          getCompartmentIndex                          ----
#_______________________________________________________________________________

#' Get the compartment index for the specified compartment name.
#' 
#' @param object generic object that contains compartments information
#' @param name compartment name
#' @return the corresponding compartment index
#' @export
getCompartmentIndex <- function(object, name) {
  stop("No default function is provided")
}

setGeneric("getCompartmentIndex", function(object, name) {
  standardGeneric("getCompartmentIndex")
})

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

#' Get equation from code record(s).
#' 
#' @param object object containing code record(s)
#' @param lhs left-hand-side variable
#' @return equation, character or NULL if not found
#' @export
getEquation <- function(object, lhs) {
  stop("No default function is provided")
}

setGeneric("getEquation", function(object, lhs) {
  standardGeneric("getEquation")
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
#----                           removeEquation                              ----
#_______________________________________________________________________________

#' Remove equation from code record.
#' 
#' @param object object containing code record(s)
#' @param lhs left-hand-side variable
#' @return updated code record
#' @export
removeEquation <- function(object, lhs) {
  stop("No default function is provided")
}

setGeneric("removeEquation", function(object, lhs) {
  standardGeneric("removeEquation")
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

#' Replace equation in code records.
#' 
#' @param object object containing code records
#' @param lhs left-hand-side variable
#' @param rhs right-hand-side expression
#' @return updated code records
#' @export
replaceEquation <- function(object, lhs, rhs) {
  stop("No default function is provided")
}

setGeneric("replaceEquation", function(object, lhs, rhs) {
  standardGeneric("replaceEquation")
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

#' Select.
#' 
#' @param object generic object
#' @param ... arguments to select
#' @return filtered object
#' @export
select <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("select", function(object, ...) {
  standardGeneric("select")
})

setMethod("select", signature=c("data.frame"), definition=function(object, ...) {
  return(return(dplyr::select(.data=object, ...)))
})

#_______________________________________________________________________________
#----                            standardise                                ----
#_______________________________________________________________________________

#' Standardise.
#' 
#' @param object generic object
#' @param ... extra arguments needed for standardisation
#' @return standardised object
#' @export
standardise <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("standardise", function(object, ...) {
  standardGeneric("standardise")
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' ToString generic method
#' 
#' @param object generic object
#' @param ... extra arguments needed for toString conversion
#' @return character value/vector
#' @export
toString <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("toString", function(object, ...) {
  standardGeneric("toString")
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
